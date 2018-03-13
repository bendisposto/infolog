package de.hhu.infolog.doc;

import java.util.List;
import java.util.Iterator;
import java.util.Map;
import java.io.File;
import java.io.FileWriter;
import java.io.FileReader;
import java.io.Writer;
import java.io.IOException;
import java.nio.file.Path;
import java.nio.file.Paths;

/** This class generates an HTML file containing the
 *  gathered documentation about a module. */

public class HtmlGenerator implements Generator {

    private Parser parser;
    private Joiner joiner;

    public HtmlGenerator(Parser parser) {
        this.parser = parser;
        this.joiner = new Joiner(parser);
        joiner.join();
    }

    /** A generator for module and predicate indices */
    public static class IndexGen implements IndexGenerator {
        public void generateModulesIndex(List<IndexStore.ModuleEntry> modules,
                                         File outdir) throws IOException {
            if(outdir.isDirectory()) {
                File outfile = new File(outdir, "modules.html");
                Path outdirp = Paths.get(outdir.getPath());
                FileWriter fw = new FileWriter(outfile);
                try {
                    fw.write("<!doctype html>\n<html><head><title>Modules</title>");
                    incorporateCss(fw);
                    fw.write("</head>\n<body>");
                    if(Preferences.get().hasFlag(Preferences.Flag.GeneratePredicatesIndex)) {
                        fw.write("<a href=\"predicates.html\" target=\"docdisp\">Predicates</a><br><br>Modules:<br>");
                    }
                    fw.write("<ul class=\"modulelist\">");
                    for(IndexStore.ModuleEntry e : modules) {
                        fw.write("<li><a href=\"");
                        fw.write(outdirp.relativize(Paths.get(e.getDocumentationPath())).toString());
                        fw.write("\" target=\"docdisp\">");
                        fw.write(e.getModuleName());
                        fw.write("</a>\n");
                    }
                    fw.write("</ul></body></html>");
                } finally {
                    fw.close();
                }
            }
        }
        public void generatePredicateIndex(List<IndexStore.PredicateEntry> preds,
                                         File outdir) throws IOException {
            if(outdir.isDirectory()) {
                File outfile = new File(outdir, "predicates.html");
                Path outdirp = Paths.get(outdir.getPath());
                FileWriter fw = new FileWriter(outfile);
                try {
                    fw.write("<!doctype html>\n<html><head><title>Predicates</title>");
                    incorporateCss(fw);
                    fw.write("</head>\n<body><ul class=\"predlist\">");
                    for(IndexStore.PredicateEntry e : preds) {
                        fw.write("<li><a href=\"");
                        fw.write(outdirp.relativize(Paths.get(e.getModule().getDocumentationPath())).toString());
                        fw.write("#");
                        fw.write(createAnchorName(e.getPredicate()));
                        fw.write("\">");
                        fw.write(e.getNameAndArity());
                        fw.write("</a>\n");
                    }
                    fw.write("</ul></body></html>");
                } finally {
                    fw.close();
                }
            }
        }
        public void generateStartPage(File outdir) throws IOException {
            if(outdir.isDirectory()) {
                File outfile = new File(outdir, "index.html");
                FileWriter fw = new FileWriter(outfile);
                try {
                    fw.write("<!doctype html>\n<html><head><title>Infolog</title>");
                    fw.write("</head>\n<frameset cols=\"20%,80%\">\n");
                    fw.write("<frame src=\"modules.html\" name=\"modulelist\">\n");
                    fw.write("<frame src=\"about:blank\" name=\"docdisp\">\n");
                    fw.write("</frameset></html>");
                } finally {
                    fw.close();
                }
            }
        }
    }

    /** A factory for creating an HtmlGenerator */
    public static class Factory implements GeneratorFactory {
        public Generator module(Parser parser) {
            return new HtmlGenerator(parser);
        }
        public IndexGenerator index() {
            return new IndexGen();
        }
    }

    /** Generate the HTML file */
    public void generate(String fileName) throws IOException {
        FileWriter fw = new FileWriter(fileName);
        try {
            fw.write("<!doctype html>\n<html><head><title>");
            if(parser.isModule()) {
                fw.write(parser.getModuleName());
            } else {
                fw.write(parser.getFileName());
            }
            fw.write(" - Infolog docs");
            fw.write("</title>");
            incorporateCss(fw);
            fw.write("</head>\n<body><h1 class=\"module\">");
            if(parser.isModule()) {
                fw.write("Module ");
                fw.write(parser.getModuleName());
            } else {
                fw.write(parser.getFileName());
            }
            if(parser.isModule()) {
                fw.write("</h1>\n");
                JoinedComment moduleComment = joiner.getModuleComment();
                if(moduleComment != null) {
                    fw.write(maskString(moduleComment.getCommentText()));
                }
                for(IndexStore.ProblemEntry problem : IndexStore.get().getModuleProblems(parser.getModuleName())) {
                    renderProblem(problem,fw);
                }
                fw.write("<h2 class=\"predgroup\">Exported predicates</h2>\n");
                for(Parser.PredicateArity pa : parser.getExportedPredicates()) {
                    Predicate p = joiner.lookup(pa);
                    if(p != null) {
                        renderPredicate(fw,p);
                    }
                }
                if(Preferences.get().hasFlag(Preferences.Flag.ListPrivatePredicates)) {
                    fw.write("<h2 class=\"predgroup\">Private predicates</h2>\n");
                    for(Predicate p : joiner) {
                        if(!parser.getExportedPredicates().contains(new Parser.PredicateArity(p.getPredicateName(), p.getArity()))) {
                            renderPredicate(fw,p);
                        }
                    }
                }
            } else {
                fw.write("</h1><h2 class=\"predgroup\">Predicates</h2>\n");
                for(Predicate p : joiner) {
                    renderPredicate(fw,p);
                }
            }
            fw.write("</body></html>");
        } finally {
            fw.close();
        }
    }

    /** Render a section about the given predicate to the HTML file */
    private void renderPredicate(Writer w, Predicate p) throws IOException{
        w.write("<a class=\"predicate\" name=\"");
        w.write(createAnchorName(p));
        w.write("\">\n");
        w.write("<h3 class=\"predname\">");
        w.write(maskString(p.getPredicateName()));
        w.write("/");
        w.write(Integer.toString(p.getArity()));
        if(p.hasComment() && !p.getComment().getParameters().isEmpty()) {
            w.write(" (");
            boolean first = true;
            // list the named parameters
            for(String param : p.getComment().getParameters()) {
                if(!first) {
                    w.write(", ");
                }
                first = false;
                w.write(maskString(param));
            }
            // call the unspecified parameters "_"
            for(int i = p.getComment().getParameters().size();
                i < p.getArity(); i++) {
                w.write(", _");
            }
            w.write(")");
        }
        w.write("</h3>\n");
        
        if(p.hasComment()) {
            w.write(maskString(p.getComment().getDescription()));
            boolean tableOpen = false;
            // Write miscellaneous tags
            for(Map.Entry<String,String> tags : p.getComment()) {
                if(!tableOpen) {
                    w.write("<table class=\"tagtable\">");
                    tableOpen = true;
                }
                w.write("<tr><td class=\"tagname\">");
                w.write(maskString(tags.getKey()));
                w.write("</td><td class=\"tagvalue\">");
                w.write(maskString(tags.getValue()));
                w.write("</td></tr>\n");
            }
            // Write parameter descriptions
            for(String param : p.getComment().getParameters()) {
                if(!tableOpen) {
                    w.write("<table class=\"tagtable\">");
                    tableOpen = true;
                }
                w.write("<tr><td class=\"tagname\">param ");
                w.write(maskString(param));
                w.write("</td><td class=\"tagvalue\">");
                w.write(maskString(p.getComment().getParamDesc(param)));
                w.write("</td></tr>\n");
            }
            // Write meta-predicate justifications
            for(Map.Entry<String,String> tags : p.getComment().getJustifications()) {
                if(!tableOpen) {
                    w.write("<table class=\"tagtable\">");
                    tableOpen = true;
                }
                w.write("<tr><td class=\"tagname\">justify ");
                w.write(maskString(tags.getKey()));
                w.write("</td><td class=\"tagvalue\">");
                w.write(maskString(tags.getValue()));
                w.write("</td></tr>\n");
            }
            // Write side effect descriptions
            for(Map.Entry<String,String> tags : p.getComment().getSideEffects()) {
                if(!tableOpen) {
                    w.write("<table class=\"tagtable\">");
                    tableOpen = true;
                }
                w.write("<tr><td class=\"tagname\">sideeffect ");
                w.write(maskString(tags.getKey()));
                w.write("</td><td class=\"tagvalue\">");
                w.write(maskString(tags.getValue()));
                w.write("</td></tr>\n");
            }
            if(tableOpen) {
                w.write("</table>");
            }
        } else {
            w.write("Unfortunately there is no documentation comment for this predicate :-(");
        }
        for(IndexStore.ProblemEntry problem : IndexStore.get().getPredicateProblems(parser.isModule() ? parser.getModuleName() : "user",
                                                                                    String.format("%s/%d",p.getPredicateName(),p.getArity()))) {
            renderProblem(problem, w);
        }
        w.write("</a>");
    }

    /** Render a problem section to the HTML file */
    private void renderProblem(IndexStore.ProblemEntry pe, Writer w) throws IOException {
        w.write("<div class=\"problem\"><span class=\"problemseverity\">");
        w.write(maskString(pe.getSeverity()));
        w.write(": </span><span class=\"problemtype\">");
        w.write(maskString(pe.getProblemType()));
        w.write(": </span><span class=\"problemmessage\">");
        w.write(maskString(pe.getMessage()));
        w.write("</span></div>\n");
    }

    /** Mask characters for use in HTML: <, >, & */
    private static String maskString(String str) {
        return str
            .replace("&","&amp;")
            .replace("<","&lt;")
            .replace(">","&gt;");
    }

    /** Incorporate CSS style sheet */
    private static void incorporateCss(Writer w) throws IOException {
        String csspath = Preferences.get().getCssFilePath();
        if(csspath != null) {
            FileReader r = new FileReader(csspath);
            char[] cbuf = new char[40];
            int len = 0;
            w.write("<style type=\"text/css\">\n");
            while((len = r.read(cbuf,0,40)) > 0) {
                w.write(cbuf,0,len);
            }
            w.write("</style>\n");
        }
    }

    /** Create an anchor name for a predicate */
    private static String createAnchorName(Predicate p) {
        return String.format("pred-%s-%d",
                             p.getPredicateName().replace("_","-"),
                             p.getArity());
    }

    public Iterator<Predicate> iterator() {
        return joiner.iterator();
    }
    
}
