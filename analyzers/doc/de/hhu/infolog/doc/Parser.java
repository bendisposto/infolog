package de.hhu.infolog.doc;

import java.util.*;
import java.io.InputStream;
import java.io.Reader;
import java.io.BufferedReader;
import java.io.IOException;

/** Chunks in the context of this program are:
 *  - line comments
 *  - block comments
 *  - clause heads
 *  - clause bodies
 * That's all. We don't do details. */

public class Parser implements Iterable<Chunk> {
    
    private Reader reader;   // the used reader
    private char[] cbuf;     // holding $cmax characters from the reader
    private Stack<Character> repushed; // holding characters that had been skipped and pushed back
    private boolean iseof;   // is it over now?
    private int cidx,        // current character index
        cmax,                // current buffer length (<= BUF_SIZE)
        linenum,             // current line number
        colnum;              // current column number
    private String fileName; // file name
    private StringBuilder chunkYet; // the already-visited part of the current chunk

    /** how many characters should cbuf hold at maximum? */
    protected static final int BUF_SIZE = 40;
    /** Symbol to return if EOF is reached */
    protected static final char EOF_SYMBOL = '\3'; // ASCII 'end of text' (ETX)
    /** Line-ending character (\n for Unix; \r or \n for Windows; \r for some other systems) */
    protected static final char LINE_END = '\n'; // ASCII 'line feed' (LF)
    /** Characters that should be interpreted as white space */
    protected static final String WHITE_SPACES = " \r\n\t";

    /** Class storing predicate name and arity */
    public static class PredicateArity {
        private String name;
        private int arity;
        public PredicateArity(String name, int arity) {
            this.name = name;
            this.arity = arity;
        }
        public String getName() { return name; }
        public int getArity() { return arity; }
        public int hashCode() {
            return (name+"/"+Integer.toString(arity)).hashCode();
        }
        public boolean equals(Object o) {
            if(o instanceof PredicateArity) {
                PredicateArity pa = (PredicateArity)o;
                return pa.arity == this.arity && pa.name.equals(this.name);
            } else return false;
        }
    }
    
    // extracted module information
    private boolean ismodule = false;
    private String moduleName = "";
    private List<PredicateArity> exportedPredicates = null;

    /** Exception class for parsing failures */
    public class ParseException extends RuntimeException {
        public ParseException(String message) {
            super(String.format("[%s %d:%d] %s", fileName, linenum, colnum, message));
        }
    }
    
    public Parser(String fileName, Reader reader) {
        this.reader = reader;
        this.fileName = fileName;
        cbuf = new char[BUF_SIZE];
        repushed = new Stack<Character>();
        linenum = 1;
        colnum = 1;
        refeed();
        skipWhite();
    }

    /** Is it over yet? */
    public boolean isEof() {
        skipWhite();
        return iseof;
    }

    /** What file are we parsing? */
    public String getFileName() {
        return fileName;
    }

    /** Get the current character */
    protected char peek() {
        if(iseof) {
            return EOF_SYMBOL;
        } else if(repushed.empty()) {
            return cbuf[cidx];
        } else {
            return repushed.peek();
        }
    }

    /** Get a character ahead of the current-position */
    protected char peek(int lookAhead) {
        Stack<Character> skipped = new Stack<Character>();
        // Scroll through the buffer
        while(lookAhead > 0 && !iseof) {
            skipped.push(peek());
            silentAdvance();
            lookAhead--;
        }
        // Find the wanted char!
        char result = peek();
        // Push back all the skipped characters
        while(!skipped.empty()) {
            repushed.push(skipped.pop());
        }
        return result;
    }

    /** Refill the buffer with more input */
    protected void refeed() {
        try {
            cmax = reader.read(cbuf, 0, BUF_SIZE);
            cidx = 0;
            if(cmax == -1) {
                iseof = true;
            }
        } catch(IOException e) {
            e.printStackTrace();
            // Recover from exception by assuming
            // that the input stream ended.
            iseof = true;
        }
    }

    /** Advance to next character without modifying line/col numbers and recorder */
    protected void silentAdvance() {
        if(repushed.empty()) {
            cidx++;
            if(cidx >= cmax) {
                refeed();
            }
        } else {
            repushed.pop();
        }
    }

    /** Advance to next character */
    protected void advance() {
        char current = peek();
        if(chunkYet != null) {
            chunkYet.append(current);
        }
        if(current == LINE_END) {
            linenum++;
            colnum = 1;
        } else {
            colnum ++;
        }
        silentAdvance();
    }

    /** Match the given character; throw an exception on failure */
    protected void match(char c) {
        if(iseof) {
            throw new ParseException(String.format("'%c' expected but EOF found.", c));
        }
        else if(c != peek()) {
            throw new ParseException(String.format("'%c' expected but '%c' found.", c, peek()));
        }
        advance();
    }
    
    /** Is the current char an ASCII letter? */
    protected boolean isAlpha() {
        return isAlpha(peek());
    }

    /** Is the given char an ASCII letter? */
    protected static boolean isAlpha(char c) {
        return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z');
    }

    /** Is the current char an arabic decimal digit? */
    protected boolean isDigit() {
        return isDigit(peek());
    }

    /** Is the given char an arabic decimal digit? */
    protected static boolean isDigit(char c) {
        return (c >= '0' && c <= '9');
    }

    /** Start recording the chunk text */
    protected void startRecording() {
        chunkYet = new StringBuilder();
    }

    /** Stop recording the chunk text and return the result */
    protected String stopRecording() {
        String result = chunkYet.toString();
        chunkYet = null;
        return result;
    }

    /** Skip any whitespace */
    protected void skipWhite() {
        char c = peek();
        while(WHITE_SPACES.indexOf(c) >= 0) {
            advance();
            c = peek();
        }
    }

    /** Read a Prolog identifier */
    protected String readIdentifier() {
        skipWhite();
        StringBuilder id = new StringBuilder("");
        char c = peek(), last = '\0';
        boolean inQuote = false;
        while(isAlpha(c) || c == '_' || isDigit(c) || c == '~' || (c == ':' && peek(1) != '-') || inQuote || c == '\'') {
            if(c == '\'' && !(inQuote && last == '\\')) {
                inQuote = !inQuote;
            } else {
                id.append(c);
            }
            advance();
            last = c;
            c = peek();
        }
        return id.toString();
    }

    /** Read a decimal number */
    protected int readNumber() {
        skipWhite();
        StringBuilder number = new StringBuilder();
        while(isDigit()) {
            number.append(peek());
            advance();
        }
        return Integer.parseInt(number.toString());
    }

    /** Read a chunk */
    public Chunk readChunk() {
        skipWhite();
        startRecording();
        int startLine = linenum,
            startCol = colnum;

        // Block comment
        if(peek() == '/' && peek(1) == '*') {
            skipBlockComment();
            String chunkText = stopRecording();
            return new BlockCommentChunk(startLine, startCol, linenum, colnum, fileName, chunkText);
        }

        // Line commment
        else if(peek() == '%') {
            skipLineComment();
            String chunkText = stopRecording();
            return new LineCommentChunk(startLine, startCol, linenum, colnum, fileName, chunkText);
        }

        // clause body
        else if(peek() == ':' && peek(1) == '-') {
            advance(); advance();
            skipClauseBody();
            String chunkText = stopRecording();
            return new ClauseBodyChunk(startLine, startCol, linenum, colnum, fileName, chunkText, false);
        }

        // DCG body
        else if(peek() == '-' && peek(1) == '-' && peek(2) == '>') {
            advance(); advance(); advance();
            skipClauseBody();
            String chunkText = stopRecording();
            return new ClauseBodyChunk(startLine, startCol, linenum, colnum, fileName, chunkText, true);
        }

        // clause head
        else if(isAlpha() || peek() == '\'') {
            PredicateArity head = skipClauseHead();
            skipWhite();
            boolean closed = false;
            if(peek() == '.') {
                advance();
                closed = true;
            }
            String chunkText = stopRecording();
            return new ClauseHeadChunk(startLine, startCol, linenum, colnum, fileName, chunkText, head.getName(), head.getArity(), closed);
        }

        // Cannot detect chunk type
        else {
            skipClauseBody(); // drop it like it's bod(y)
            String chunkText = stopRecording();
            return new Chunk(startLine, startCol, linenum, colnum, fileName, chunkText, ChunkType.Bogus);
        }
    }

    /** Skip a block comment */
    protected void skipBlockComment() {
        advance(); advance();
        while(!(peek() == '*' && peek(1) == '/') && !iseof) {
            advance();
        }
        advance(); advance();
    }

    /** Skip a line comment */
    protected void skipLineComment() {
        advance();
        while(!(peek() == LINE_END) && !iseof) {
            advance();
        }
        advance();
    }

    /** Skip any comments */
    protected void skipComments() {
        boolean changed = true;
        while(changed) {
            changed = false;
            skipWhite();
            if(peek() == '%') {
                skipLineComment();
                changed = true;
            } else if(peek() == '/' && peek(1) == '*') {
                skipBlockComment();
                changed = true;
            }
        }
    }
    
    /** Skip a clause body */
    protected void skipClauseBody() {
        skipWhite();
        while(peek() != '.' && !iseof) {
            if (peek() == ',') {
                advance();
                skipWhite();
            }
            if(isAlpha()) {
                String id = readIdentifier();
                if(id.equals("module")) {
                    readModulePred();
                }
            }
            skipValue();
        }
        advance();
    }

    /** Read module name and exports */
    protected void readModulePred() {
        match('(');
        skipComments();
        ismodule = true;
        moduleName = readIdentifier();
        skipComments(); skipWhite();
        match(',');
        skipComments(); skipWhite();
        match('[');
        skipComments(); skipWhite();
        exportedPredicates = new LinkedList<PredicateArity>();
        while(peek()!=']') {
            boolean parens = false;
            skipComments();
            skipWhite();
            if(peek() == '(') {
                parens = true;
                advance();
                skipComments();
            }
            String predName = readIdentifier();
            skipComments();
            skipWhite();
            if(parens) {
                match(')');
                skipComments();
                skipWhite();
            }
            match('/');
            int arity = readNumber();
            skipComments();
            exportedPredicates.add(new PredicateArity(predName,arity));
            skipWhite();
            if(peek() != ']') match(',');
        }
        advance();
        skipComments();
        skipWhite();
        match(')');
    }

    /** Skip a clause head */
    protected PredicateArity skipClauseHead() {
        String predName = readIdentifier();
        int arity = 0;
        skipWhite();
        if(peek() == '(') {
            advance();
            arity++;
            skipValue();
            skipWhite();
            while(peek() == ',') {
                advance();
                arity++;
                skipValue();
                skipWhite();
            }
            match(')');
        }
        return new PredicateArity(predName, arity);
    }

    /** Skip a Prolog value (atoms, functors, strings, lists) */
    protected void skipValue() {
        boolean inQuote = false;
        boolean inComment = false;
        char quoteChar = '\0', commentChar = '\0';
        int parenLevel = 0;
        char c = peek(), last = '\2';

        // simple DFA
        while(!iseof && !(parenLevel == 0 && (c == ',' || c == ')' || (c == '.' && peek(1)!='.' && last != '.' && !isDigit(peek(1)))) && !inQuote && !inComment)) {
            if(inQuote && c == quoteChar && last != '\\') {
                inQuote = false;
            } else if (!inQuote && !inComment && (c == '\'' || c == '"')) {
                inQuote = true;
                quoteChar = c;
            } else if (!inQuote && !inComment && (c == '(' || c == '[' || c == '{')) {
                parenLevel++;
            } else if (!inQuote && !inComment && (c == ')' || c == ']' || c == '}')) {
                parenLevel--;
            } else if (!inQuote && !inComment && last == '/' && c == '*') {
                inComment = true;
                commentChar = '*';
            } else if (inComment && last == '*' && c == '/' && commentChar == '*') {
                inComment = false;
            } else if (inComment && c == LINE_END && commentChar == '%') {
                inComment = false;
            } else if (!inQuote && !inComment && c == '%') {
                inComment = true;
                commentChar = '%';
            }

            advance();
            last = c;
            c = peek();
        }
    }

    /** Chunk iterator */
    public Iterator<Chunk> iterator() {
        return new Iterator<Chunk>() {
            public boolean hasNext() {
                return !isEof();
            }
            
            public Chunk next() {
                return readChunk();
            }
        };
    }

    /** Is the parsed file a module? */
    public boolean isModule() {
        return ismodule;
    }

    /** Get the module name */
    public String getModuleName() {
        return moduleName;
    }

    /** Exported predicates */
    public List<PredicateArity> getExportedPredicates() {
        return exportedPredicates;
    }
    
}
