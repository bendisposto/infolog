package de.hhu.infolog.doc;

import java.util.Set;
import java.util.HashSet;

/** This singleton class stores all the preferences set by command-line
    options. */

public class Preferences {

    private static Preferences instance = new Preferences();
    /** Get the singleton instance */
    public static Preferences get() {
        return instance;
    }

    // Preference flags
    public static enum Flag {
        /** List private predicates of modules? Otherwise: only exported predicates */
        ListPrivatePredicates,
        /** Start LaTeX files with a preamble */
        LatexPreamble,
        /** Generate a modules index */
        GenerateModulesIndex,
        /** Generate a predicate index */
        GeneratePredicatesIndex,
        /** Generate a start page */
        GenerateStartPage
    }
    private Set<Flag> flags = new HashSet<>();
    /** Set a flag. */
    public void setFlag(Flag flag) {
        flags.add(flag);
    }
    /** Unset a flag. */
    public void unsetFlag(Flag flag) {
        flags.remove(flag);
    }
    /** Get the current status of a flag (set/unset) */
    public boolean hasFlag(Flag flag) {
        return flags.contains(flag);
    }
    
    
    // The factory for the documentation generators
    private GeneratorFactory factory;
    /** Set the document generator factory (e.g. HtmlGenerator.Factory) */
    public void setGeneratorFactory(GeneratorFactory factory) {
        this.factory = factory;
    }
    /** Get the current document generator factory */
    public GeneratorFactory getGeneratorFactory() {
        return factory;
    }

    // File extension for output files
    private String fileext = ".out";
    /** Set the file name extension for documentation files */
    public void setFileExtension(String fileext) {
        this.fileext = fileext;
    }
    /** Get the current file name extension for documentation files */
    public String getFileExtension() {
        return fileext;
    }

    // CSS file name
    private String cssfile = null;
    /** Set the file path to the style sheet */
    public void setCssFilePath(String cssfile) {
        this.cssfile = cssfile;
    }
    /** Get the current style sheet file path */
    public String getCssFilePath() {
        return cssfile;
    }

    // Prolog target file name
    private String plfile = null;
    /** Set the file path for the Prolog export */
    public void setPrologFilePath(String plfile) {
        this.plfile = plfile;
    }
    /** Get the current Prolog export file path */
    public String getPrologFilePath() {
        return plfile;
    }

    // infolog_problems.csv file path
    private String problemscsv = null;
    /** Set the file path for infolog_problems.csv */
    public void setProblemsCsvFilePath(String problemscsv) {
        this.problemscsv = problemscsv;
    }
    /** Get the current file path for infolog_problems.csv */
    public String getProblemsCsvFilePath() {
        return problemscsv;
    }
}
