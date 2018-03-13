package de.hhu.infolog.doc;

import java.io.File;
import java.io.FileFilter;
import java.io.FileReader;
import java.io.IOException;

/** This class traverses a directory and creates documentation
 *  for all found files */

public class DirectoryTraverser {

    private File inputDir, outputDir;

    public DirectoryTraverser(File inputDir, File outputDir) {
        this.inputDir = inputDir;
        this.outputDir = outputDir;
    }

    public DirectoryTraverser(String inputDir, String outputDir) {
        this.inputDir = new File(inputDir);
        this.outputDir = new File(outputDir);
    }

    /** Traverse through the directories, process prolog files,
     *  and also generate a module index page */
    public void process() throws IOException {
        String csvpath = Preferences.get().getProblemsCsvFilePath();
        if(csvpath != null) {
            ProblemImporter pi = new ProblemImporter();
            pi.fromCsv(csvpath);
        }
        process(inputDir, outputDir);
        processModuleIndex();
        String plpath = Preferences.get().getPrologFilePath();
        if(plpath != null) {
            PrologExporter exporter = new PrologExporter();
            exporter.export(plpath);
        }
    }
    
    private void process(File infile, File outfile) throws IOException {
        if(infile.isFile()) {
            Parser p = new Parser(infile.getName(), new FileReader(infile));
            GeneratorFactory factory = Preferences.get().getGeneratorFactory();
            Iterable<Predicate> predicates;
            if(factory != null) {
                Generator gen = factory.module(p);
                gen.generate(outfile.getPath());
                predicates = gen;
            } else {
                Joiner j = new Joiner(p);
                j.join();
                predicates = j;
            }
            String modname = p.isModule() ? p.getModuleName() : infile.getName();
            IndexStore.get().addModule(modname,
                                       infile.getPath(),
                                       outfile.getPath());
            for(Predicate pred : predicates) {
                IndexStore.get().addPredicate(modname, pred);
            }
        } else if(infile.isDirectory()) {
            File[] files = infile.listFiles(new FileFilter() {
                    public boolean accept(File file) {
                        return file.isDirectory() || file.getName().endsWith(".pl");
                    }
                });
            for(File f : files) {
                if(f.isFile()) {
                    process(f,new File(outfile,
                                       f.getName()+Preferences.get().getFileExtension()));
                } else {
                    File outdir = new File(outfile, f.getName());
                    if(!outdir.exists()) {
                        outdir.mkdirs();
                    }
                    process(f, outdir);
                }
            }
        }
    }

    private void processModuleIndex() throws IOException {
        GeneratorFactory factory = Preferences.get().getGeneratorFactory();
        if(factory != null) {
            IndexGenerator gen = factory.index();
            if(gen != null) {
                if(Preferences.get().hasFlag(Preferences.Flag.GenerateModulesIndex)) {
                    gen.generateModulesIndex(IndexStore.get().getSortedModules(new IndexStore.ModulesByName()), outputDir);
                }
                if(Preferences.get().hasFlag(Preferences.Flag.GeneratePredicatesIndex)) {
                    gen.generatePredicateIndex(IndexStore.get().getSortedPredicates(new IndexStore.PredicatesByName()), outputDir);
                }
                if(Preferences.get().hasFlag(Preferences.Flag.GenerateStartPage)) {
                    gen.generateStartPage(outputDir);
                }
            }
        }
    }
}
