package de.hhu.infolog.doc;

/** This class represents a code chunk as emitted by Parser */

public class Chunk {

    private int startLine, startCol, endLine, endCol;
    private String fileName, chunkText;
    private ChunkType chunkType;

    public Chunk(int startLine, int startCol,
                 int endLine, int endCol,
                 String fileName,String chunkText,
                 ChunkType chunkType) {
        this.startLine = startLine;
        this.startCol = startCol;
        this.endLine = endLine;
        this.endCol = endCol;
        this.fileName = fileName;
        this.chunkText = chunkText;
        this.chunkType = chunkType;
    }
    
    /** Line number, where the code chunk starts */
    public int getStartLine() {
        return startLine;
    }

    /** Column number, where the code chunk starts */
    public int getStartColumn() {
        return startCol;
    }

    /** Line number, where the code chunk ends */
    public int getEndLine() {
        return endLine;
    }

    /** Column number, where the code chunk ends */
    public int getEndColumn() {
        return endCol;
    }

    /** The file name where this code chunk was found */
    public String getFileName() {
        return fileName;
    }

    /** Get the content of this code chunk */
    public String getChunkText() {
        return chunkText;
    }

    /** Get the chunk type */
    public ChunkType getChunkType() {
        return chunkType;
    }

    public String toString() {
        return String.format("[%s %d:%d - %d:%d] %s: %s", getFileName(), getStartLine(), getStartColumn(), getEndLine(), getEndColumn(), getChunkType().toString(), getChunkText());
    }
}
