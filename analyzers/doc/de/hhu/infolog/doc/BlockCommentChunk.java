package de.hhu.infolog.doc;

/** This class represents a code chunk of type BlockComment */

public class BlockCommentChunk extends CommentChunk {

    private boolean starred = false;
    
    public BlockCommentChunk(int startLine, int startCol,
                             int endLine, int endCol,
                             String fileName, String chunkText) {
        super(startLine, startCol, endLine, endCol,
              fileName, chunkText,
              "", ChunkType.BlockComment);
        commentText = extractCommentText(chunkText);
    }

    private String extractCommentText(String chunkText) {
        // split into lines
        String[] lines = chunkText
            .substring(2,chunkText.length()-2)
            .trim()
            .split("\n");
        // single line => we're done
        if(lines.length == 1) return lines[0];
        // is it a starred comment?
        boolean isStarred = true;
        for(int i = 1; i < lines.length; i++) {
            isStarred = isStarred && lines[i].startsWith(" *");
        }
        this.starred = isStarred;
        // accumulate
        StringBuilder accum = new StringBuilder(lines[0]);
        for(int i  = 1; i < lines.length; i++) {
            if(isStarred) {
                accum.append("\n"+lines[i].substring(2,lines[i].length()).trim());
            } else {
                accum.append("\n"+lines[i].trim());
            }
        }
        return accum.toString();
    }

    public String toString() {
        return String.format("[%s %d:%d - %d:%d] %sblock comment: %s", getFileName(), getStartLine(), getStartColumn(), getEndLine(), getEndColumn(), starred? "starred " : "",  getCommentText());
    }

    /** Does every line start with an asterisk? */
    public boolean isStarred() {
        return starred;
    }
    
}
