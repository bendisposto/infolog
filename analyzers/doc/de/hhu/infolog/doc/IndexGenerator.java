package de.hhu.infolog.doc;

import java.util.List;
import java.io.File;
import java.io.IOException;

/** An interface for classes that can generate a module and predicate index. */

public interface IndexGenerator {

    /** Generate a module index */
    public void generateModulesIndex(List<IndexStore.ModuleEntry> modules,
                                     File outdir) throws IOException;

    /** Generate a start page (possibly a frameset) */
    public void generateStartPage(File outdir) throws IOException;

    /** Generate a predicate index */
    public void generatePredicateIndex(List<IndexStore.PredicateEntry> preds,
                                       File outdir) throws IOException;
    
}
