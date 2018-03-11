package de.hhu.infolog.doc;

import java.io.IOException;

/** Implementors of this interface are classes that can create
 *  documentation files. */

public interface Generator extends Iterable<Predicate> {

    /** Create the file */
    public void generate(String fileName) throws IOException;
    
}
