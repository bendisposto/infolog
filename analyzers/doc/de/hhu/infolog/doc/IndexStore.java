package de.hhu.infolog.doc;

import java.util.HashMap;
import java.util.Map;
import java.util.HashSet;
import java.util.Set;
import java.util.Iterator;
import java.util.List;
import java.util.LinkedList;
import java.util.Comparator;

/** Saves all module names, pathes and predicates, for generating index 
 *  files. */

public class IndexStore implements Iterable<IndexStore.ModuleEntry> {

    /** A module entry */
    public static class ModuleEntry {
        
        private String moduleName;
        private String filePath;
        private String docPath;

        public ModuleEntry(String moduleName, String filePath, String docPath) {
            this.moduleName = moduleName;
            this.filePath = filePath;
            this.docPath = docPath;
        }

        /** Get the module's reference name */
        public String getModuleName() {
            return moduleName;
        }

        /** Get the module's file path */
        public String getFilePath() {
            return filePath;
        }

        /** Get the module's documentation file path */
        public String getDocumentationPath() {
            return docPath;
        }

        public int hashCode() {
            return filePath.hashCode();
        }

        public boolean equals(Object o) {
            if(o instanceof ModuleEntry) {
                ModuleEntry e = (ModuleEntry)o;
                return e.filePath.equals(filePath);
            } else {
                return false;
            }
        }

    }

    /** A predicate entry */
    public static class PredicateEntry {

        private Predicate pred;
        private ModuleEntry mod;

        public PredicateEntry(ModuleEntry mod, Predicate pred) {
            this.pred = pred;
            this.mod = mod;
        }

        /** Get the underlying predicate */
        public Predicate getPredicate() {
            return pred;
        }

        /** Get the underlying predicate's name */
        public String getName() {
            return pred.getPredicateName();
        }

        /** Get the underlying predicate's arity */
        public int getArity() {
            return pred.getArity();
        }

        /** Get the underlying predicate's name and arity */
        public String getNameAndArity() {
            return String.format("%s/%d", pred.getPredicateName(), pred.getArity());
        }

        /** Get the module entry */
        public ModuleEntry getModule() {
            return mod;
        }

        /** Get the module:predicate/arity identifier */
        public String getModulePredicateArity() {
            return String.format("%s:%s/%d", mod.getModuleName().endsWith(".pl") ? "user" : mod.getModuleName(), pred.getPredicateName(), pred.getArity());
        }

        /** Get masked module:predicate/arity identifier for use in the
         * PrologExporter */
        public String getMaskedModulePredicateArity() {
            return String.format("%s:'%s'/%d", mod.getModuleName().endsWith(".pl") ? "user" : mod.getModuleName(), pred.getPredicateName(), pred.getArity());
        }

        public int hashCode() {
            return getModulePredicateArity().hashCode();
        }

        public boolean equals(Object o) {
            if(o instanceof PredicateEntry) {
                PredicateEntry e = (PredicateEntry)o;
                return e.getModulePredicateArity().equals(getModulePredicateArity());
            } else {
                return false;
            }
        }
        
    }

    /** Problem entry */
    public static class ProblemEntry {
        
        private String problemType, severity, message;
        
        public ProblemEntry(String problemType, String severity, String message) {
            this.problemType = problemType;
            this.severity = severity;
            this.message = message;
        }

        /** Get the problem type */
        public String getProblemType() {
            return problemType;
        }

        /** Get the problem severity */
        public String getSeverity() {
            return severity;
        }

        /** Get the problem message */
        public String getMessage() {
            return message;
        }

        public int hashCode() {
            return String.format("%s:%s:%s",problemType,severity,message).hashCode();
        }

        public boolean equals(Object o) {
            if(o instanceof ProblemEntry) {
                ProblemEntry e = (ProblemEntry)o;
                return e.problemType.equals(problemType)
                    && e.severity.equals(severity)
                    && e.message.equals(message);
            } else {
                return false;
            }
        }
    }

    /** An alphabetic comparator for ModuleEntries */
    public static class ModulesByName implements Comparator<ModuleEntry> {
        public int compare(ModuleEntry e1, ModuleEntry e2) {
            return e1.getModuleName().compareToIgnoreCase(e2.getModuleName());
        }
    }

    /** An alphabetic comparator for PredicateEntries */
    public static class PredicatesByName implements Comparator<PredicateEntry> {
        public int compare(PredicateEntry e1, PredicateEntry e2) {
            return e1.getNameAndArity().compareToIgnoreCase(e2.getNameAndArity());
        }
    }

    private static IndexStore instance = new IndexStore();
    /** Get the singleton instance */
    public static IndexStore get() {
        return instance;
    }
    
    private Map<String,ModuleEntry> modules = new HashMap<>();
    private Map<String,PredicateEntry> predicates = new HashMap<>();
    private Map<String,Set<ProblemEntry>> problems = new HashMap<>();

    /** Add a module to the store */
    public void addModule(String moduleName, String filePath, String docPath) {
        modules.put(moduleName,
                    new ModuleEntry(moduleName, filePath, docPath));
    }

    /** Add a predicate to the store */
    public void addPredicate(String moduleName, Predicate predicate) {
        ModuleEntry me = modules.get(moduleName);
        PredicateEntry pe = new PredicateEntry(me, predicate);
        predicates.put(pe.getModulePredicateArity(), pe);
    }

    /** Add a module problem to the store */
    public void addModuleProblem(String moduleName, ProblemEntry problem) {
        if(problems.containsKey(moduleName)) {
            problems.get(moduleName).add(problem);
        } else {
            Set<ProblemEntry> set = new HashSet<>();
            set.add(problem);
            problems.put(moduleName, set);
        }
    }

    /** Add a predicate problem to the store */
    public void addPredicateProblem(String moduleName, String predName, ProblemEntry problem) {
        String key = String.format("%s:%s", moduleName, predName);
        if(problems.containsKey(key)) {
            problems.get(key).add(problem);
        } else {
            Set<ProblemEntry> set = new HashSet<>();
            set.add(problem);
            problems.put(key, set);
        }
    }
    
    public Iterator<ModuleEntry> iterator() {
        return modules.values().iterator();
    }

    /** Get sorted module entries */
    public List<ModuleEntry> getSortedModules(Comparator<ModuleEntry> comparator) {
        LinkedList<ModuleEntry> ll = new LinkedList<>(modules.values());
        ll.sort(comparator);
        return ll;
    }

    /** Get sorted predicate entries */
    public List<PredicateEntry> getSortedPredicates(Comparator<PredicateEntry> comparator) {
        LinkedList<PredicateEntry> ll = new LinkedList<>(predicates.values());
        ll.sort(comparator);
        return ll;
    }

    /** Get module problems */
    public Set<ProblemEntry> getModuleProblems(String moduleName) {
        if(problems.containsKey(moduleName)) {
            return problems.get(moduleName);
        } else {
            return new HashSet<>();
        } 
    }

    /** Get predicate problems */
    public Set<ProblemEntry> getPredicateProblems(String moduleName, String predName) {
        String key = String.format("%s:%s", moduleName, predName);
        if(problems.containsKey(key)) {
            return problems.get(key);
        } else {
            return new HashSet<>();
        } 
    }
}
