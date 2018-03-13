package de.hhu.infolog.doc;

/** A factory interface for creating generators from parsers */

public interface GeneratorFactory {

    /** Build the module documentation generator */
    public Generator module(Parser parser);

    /** Build the index generator */
    public IndexGenerator index();

}
