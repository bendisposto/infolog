package de.hhu.infolog.doc;

/** This class represents a code chunk of type LineComment */

public class LineCommentChunk extends CommentChunk {

    public LineCommentChunk(int startLine, int startCol,
                            int endLine, int endCol,
                            String fileName, String chunkText) {
        super(startLine, startCol, endLine, endCol,
              fileName, chunkText,
              extractCommentText(chunkText), ChunkType.LineComment);
    }

    private static String extractCommentText(String chunkText) {
        return chunkText.substring(1, chunkText.length()-1).trim();
    }

    public String toString() {
        return String.format("[%s %d:%d - %d:%d] line comment: %s", getFileName(), getStartLine(), getStartColumn(), getEndLine(), getEndColumn(), getCommentText());
    }
    
}
