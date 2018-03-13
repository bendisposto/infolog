package de.hhu.infolog.doc;

/** This class represents a code chunk of type ClauseHead */

public class ClauseHeadChunk extends Chunk {

    private String predName;
    private int arity;
    private boolean closed;

    public ClauseHeadChunk(int startLine, int startCol,
                           int endLine, int endCol,
                           String fileName, String chunkText,
                           String predName, int arity,
                           boolean closed) {
        super(startLine, startCol, endLine, endCol,
              fileName, chunkText,
              ChunkType.ClauseHead);
        this.predName = predName;
        this.arity = arity;
        this.closed = closed;
    }

    /** Get the predicate name */
    public String getPredicateName() {
        return predName;
    }

    /** Get the predicate arity */
    public int getArity() {
        return arity;
    }

    /** Does this clause head still expect a body? (false if already ended by a period) */
    public boolean hasBody() {
        return !closed;
    }

    public String toString() {
        return String.format("[%s %d:%d - %d:%d] clause for %s/%d", getFileName(), getStartLine(), getStartColumn(), getEndLine(), getEndColumn(), getPredicateName(), getArity());
    }
    
}
