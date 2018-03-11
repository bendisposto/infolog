package de.hhu.infolog.doc;

/** This class represents a code chunk of type LineComment or BlockComment */

public class CommentChunk extends Chunk {

    protected String commentText;

    protected CommentChunk(int startLine, int startCol,
                           int endLine, int endCol,
                           String fileName, String chunkText,
                           String commentText, ChunkType chunkType) {
        super(startLine, startCol, endLine, endCol,
              fileName, chunkText, chunkType);
        this.commentText = commentText;
    }

    /** Get the semi-processed comment text */
    public String getCommentText() {
        return commentText;
    }
    
}
