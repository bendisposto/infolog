package de.hhu.infolog.doc;

import java.util.Iterator;
import java.io.FileWriter;
import java.io.Writer;
import java.io.IOException;

/** This class generates a LaTeX file containing the
 *  gathered documentation about a module */

public class LatexGenerator implements Generator {

    private Parser parser;
    private Joiner joiner;

    public LatexGenerator(Parser parser) {
        this.parser = parser;
        this.joiner = new Joiner(parser);
        joiner.join();
    }

    /** A factory for creating a LatexGenerator */
    public static class Factory implements GeneratorFactory {
        public Generator module(Parser parser) {
            return new LatexGenerator(parser);
        }
        public IndexGenerator index() {
            return null;
        }
    }

    /** Generate the LaTeX file */
    public void generate(String fileName) throws IOException {
        FileWriter fw = new FileWriter(fileName);
        boolean preamble = Preferences.get().hasFlag(Preferences.Flag.LatexPreamble);
        try {
            // Render the preamble
            if(preamble) {
                renderPreamble(fw);
            }
            // Module/file title
            fw.write("\\module{");
            if(parser.isModule()) {
                fw.write("Module ");
                fw.write(maskString(parser.getModuleName()));
            } else {
                fw.write(maskString(parser.getFileName()));
            }
            if(parser.isModule()) {
                // Module information
                fw.write("}\n");
                JoinedComment moduleComment = joiner.getModuleComment();
                if(moduleComment != null) {
                    fw.write(maskString(moduleComment.getCommentText()));
                }
                // Exported predicates
                fw.write("\\predgroup{Exported predicates}");
                for(Parser.PredicateArity pa : parser.getExportedPredicates()) {
                    Predicate p = joiner.lookup(pa);
                    if(p != null) {
                        renderPredicate(fw,p);
                    }
                }
                // Private predicates
                if(Preferences.get().hasFlag(Preferences.Flag.ListPrivatePredicates)) {
                    fw.write("\\predgroup{Private predicates}");
                    for(Predicate p : joiner) {
                        if(!parser.getExportedPredicates().contains(new Parser.PredicateArity(p.getPredicateName(), p.getArity()))) {
                            renderPredicate(fw,p);
                        }
                    }
                }
            } else {
                // If it's not a module, list all predicates
                fw.write("}\n\\predgroup{Predicates}");
                for(Predicate p : joiner) {
                    renderPredicate(fw,p);
                }
            }
            // \end{document}
            if(preamble) {
                renderPostamble(fw);
            }
        } finally {
            fw.close();
        }
    }

    /** Render the preamble and \begin{document} to the file */
    public void renderPreamble(Writer w) throws IOException {
        w.write("\\documentclass[fontsize=12pt]{scrartcl}\n");
        w.write("\\usepackage[pdfborder={0 0 0}]{hyperref}\n");
        w.write("\\let\\module=\\section\n");
        w.write("\\let\\predgroup=\\subsection\n");
        w.write("\\let\\predicate=\\subsubsection\n");
        w.write("\\begin{document}\n");
    }

    /** Render \end{document} to the file */
    public void renderPostamble(Writer w) throws IOException {
        w.write("\\end{document}\n");
    }

    /** Render a section about the given predicate to the LaTeX file */
    private void renderPredicate(Writer w, Predicate p) throws IOException{
        w.write("\\predicate{");
        w.write(maskString(p.getPredicateName()));
        w.write("/");
        w.write(Integer.toString(p.getArity()));
        w.write("}\n");
        if(p.hasComment()) {
            w.write(maskString(p.getComment().getCommentText()));
        } else {
            w.write("Unfortunately there is no documentation comment for this predicate :-(");
        }
        w.write("\n");
    }

    /** Mask characters for use in LaTeX: \, _, $, #, ^, <, > */
    private static String maskString(String str) {
        return str
            .replace("\\","\\textbackslash ")
            .replace("{","\\{")
            .replace("}","\\}")
            .replace("&","\\&")
            .replace("_","\\textunderscore{}")
            .replace("$","\\$")
            .replace("#","\\#")
            .replace("^","\\^{}")
            .replace("<","\\textless{}")
            .replace(">","\\textgreater{}");
    }

    public Iterator<Predicate> iterator() {
        return joiner.iterator();
    }
    
}
