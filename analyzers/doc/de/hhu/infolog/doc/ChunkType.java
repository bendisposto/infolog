package de.hhu.infolog.doc;

/** Different types of chunks, as generated py Parser */

public enum ChunkType {
    /** a line comment (with %) */
    LineComment,
    /** a C-style block comment */
    BlockComment,
    /** a clause head */
    ClauseHead,
    /** a clause body */
    ClauseBody,
    /** something we could not identify */
    Bogus
}
