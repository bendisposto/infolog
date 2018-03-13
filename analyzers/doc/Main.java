import java.io.Reader;
import java.io.FileReader;
import java.io.InputStreamReader;
import java.io.IOException;
import de.hhu.infolog.doc.DirectoryTraverser;
import de.hhu.infolog.doc.HtmlGenerator;
import de.hhu.infolog.doc.LatexGenerator;
import de.hhu.infolog.doc.Preferences;

public class Main {

    public static void main(String[] args) {
        try {
            if(args.length > 0) {
                // Process the given command-line options
                int idx = 0;
                String inpath = null, outpath = null, format = "html",
                    csspath = null, plpath = null, problemsCsv = null;
                boolean listPrivatePreds = true, preamble = true,
                    modindex = true, startpage = true, predindex = true;
                while(idx < args.length) {
                    if(args[idx].equals("--out")) {
                        outpath = args[idx+1];
                        idx ++;
                    } else if(args[idx].equals("--html")) {
                        format = "html";
                    } else if(args[idx].equals("--latex")) {
                        format = "latex";
                    } else if(args[idx].equals("--no-docs")) {
                        format = null;
                    } else if(args[idx].equals("--preamble")) {
                        preamble = true;
                    } else if(args[idx].equals("--no-preamble")) {
                        preamble = false;
                    } else if(args[idx].equals("--privates-too")) {
                        listPrivatePreds = true;
                    } else if(args[idx].equals("--exports-only")) {
                        listPrivatePreds = false;
                    } else if(args[idx].equals("--start-page")) {
                        startpage = true;
                    } else if(args[idx].equals("--no-start-page")) {
                        startpage = false;
                    } else if(args[idx].equals("--modules-index")) {
                        modindex = true;
                    } else if(args[idx].equals("--no-modules-index")) {
                        modindex = false;
                    } else if(args[idx].equals("--predicates-index")) {
                        predindex = true;
                    } else if(args[idx].equals("--no-predicates-index")) {
                        predindex = false;
                    } else if(args[idx].equals("--css")) {
                        csspath = args[idx+1];
                        idx ++;
                    } else if(args[idx].equals("--export-prolog")) {
                        plpath = args[idx+1];
                        idx ++;
                    } else if(args[idx].equals("--problems-csv")) {
                        problemsCsv = args[idx+1];
                        idx ++;
                    } else {
                        inpath = args[idx];
                    }
                    idx++;
                }
                if(inpath == null) inpath = ".";
                if(outpath == null) outpath = ".";
                if(format == null) {
                    Preferences.get().setGeneratorFactory(null);
                } else if(format.equals("html")) {
                    Preferences.get().setGeneratorFactory(new HtmlGenerator.Factory());
                    Preferences.get().setFileExtension(".html");
                } else if(format.equals("latex")) {
                    Preferences.get().setGeneratorFactory(new LatexGenerator.Factory());
                    Preferences.get().setFileExtension(".tex");
                } else {
                    System.out.println("Unknown format: "+format);
                    return;
                }
                if(listPrivatePreds) {
                    Preferences.get().setFlag(Preferences.Flag.ListPrivatePredicates);
                }
                if(preamble) {
                    Preferences.get().setFlag(Preferences.Flag.LatexPreamble);
                }
                if(modindex) {
                    Preferences.get().setFlag(Preferences.Flag.GenerateModulesIndex);
                }
                if(startpage) {
                    Preferences.get().setFlag(Preferences.Flag.GenerateStartPage);
                }
                if(predindex) {
                    Preferences.get().setFlag(Preferences.Flag.GeneratePredicatesIndex);
                }
                Preferences.get().setCssFilePath(csspath);
                Preferences.get().setPrologFilePath(plpath);
                Preferences.get().setProblemsCsvFilePath(problemsCsv);

                // Actually do something!
                DirectoryTraverser trav = new DirectoryTraverser(inpath,outpath);
                trav.process();
            }
        } catch(IOException e) {
            e.printStackTrace();
        }
    }
}
