package de.hhu.infolog.doc;

/** This class represents a chunk of type ClauseBody */

public class ClauseBodyChunk extends Chunk {

    private boolean dcg;

    public ClauseBodyChunk(int startLine, int startCol,
                           int endLine, int endCol,
                           String fileName, String chunkText,
                           boolean dcg) {
        super(startLine, startCol, endLine, endCol,
              fileName, chunkText, ChunkType.ClauseBody);
        this.dcg = dcg;
    }

    /** Is it a DCG clause? */
    public boolean isDCG() {
        return dcg;
    }
    
}
