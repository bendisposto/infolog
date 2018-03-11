package de.hhu.infolog.doc;

import java.util.List;
import java.util.Map;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedList;

/** This class represents a documentation comment, either created from
 *  a block comment chunk or from many line comments */

public class JoinedComment implements Iterable<Map.Entry<String,String>> {

    private String commentText, descriptionText;
    private CommentStyle commentStyle;
    private Map<String,String> miscTagDictionary,
        justificationDictionary,
        paramDictionary,
        sideEffectDictionary;
    private List<String> paramList;

    /** Create a documentation comment from a single block comment */
    public JoinedComment(BlockCommentChunk chunk) {
        if(chunk.getChunkText().startsWith("/**")) {
            this.commentStyle = CommentStyle.JavaDoc;
            this.commentText = chunk.getCommentText().substring(1).trim();
        } else if(chunk.isStarred()) {
            this.commentStyle = CommentStyle.StarredBlock;
            this.commentText = chunk.getCommentText();
        } else {
            this.commentStyle = CommentStyle.NormalBlock;
            this.commentText = chunk.getCommentText();
        }
    }

    /** Create a documentation comment from many line comments */
    public JoinedComment(List<LineCommentChunk> chunks) {
        if(chunks.size() == 1) {
            this.commentStyle = CommentStyle.SingleLine;
        } else if(chunks.get(0).getCommentText().startsWith("%")) {
            this.commentStyle = CommentStyle.Hook;
        } else {
            this.commentStyle = CommentStyle.MultiLine;
        }
        StringBuilder accum = null;
        for(LineCommentChunk chunk : chunks) {
            if(accum == null) {
                String t = chunk.getCommentText();
                if(commentStyle == CommentStyle.Hook) {
                    t = t.substring(1);
                }
                accum = new StringBuilder(t);
            } else {
                accum.append("\n"+chunk.getCommentText());
            }
        }
        this.commentText = accum.toString().trim();
    }

    /** Get the entire comment text, including tags */
    public String getCommentText() {
        return commentText;
    }

    /** Get the comment style */
    public CommentStyle getCommentStyle() {
        return commentStyle;
    }

    /** Tag category */
    public static enum TagCategory {
        /** An @param tag */
        Parameter,
        /** An @justify tag */
        Justification,
        /** The unnamed description tag */
        Description,
        /** a @sideeffect tag */
        SideEffect,
        /** Any other tags */
        Misc
    }

    /** Add a tag to the dictionaries */
    private void addTag(TagCategory cat, String name, String value) {
        switch(cat) {
        case Misc:
            miscTagDictionary.put(name,value);
            break;
        case Description:
            descriptionText = value;
            break;
        case Justification:
            justificationDictionary.put(name,value);
            break;
        case Parameter:
            paramList.add(name);
            paramDictionary.put(name,value);
            break;
        case SideEffect:
            sideEffectDictionary.put(name,value);
            break;
        }
    }
    
    /** Process all tags specified in the comment */
    private void processTags() {
        miscTagDictionary = new HashMap<>();
        justificationDictionary = new HashMap<>();
        paramDictionary = new HashMap<>();
        paramList = new LinkedList<>();
        sideEffectDictionary = new HashMap<>();
        String[] lines = commentText.split("\n");
        String currentTag = null;
        TagCategory tagCategory = TagCategory.Description;
        StringBuilder tagContent = new StringBuilder();
        
        for(String line : lines) {
            line = line.trim();
            if(line.startsWith("@")) {
                // new tag
                line = line.substring(1).trim();
                String[] words = line.split(" ");
                if(words.length > 0) {
                    addTag(tagCategory, currentTag, tagContent.toString().trim());
                    currentTag = words[0];
                    tagCategory = TagCategory.Misc;
                    int i = 1;
                    if(words.length > 1) {
                        if(currentTag.equals("param")) {
                            currentTag = words[1];
                            tagCategory = TagCategory.Parameter;
                            i++;
                        } else if(currentTag.equals("justify")) {
                            currentTag = words[1];
                            tagCategory = TagCategory.Justification;
                            i++;
                        } else if(currentTag.equals("sideeffect")) {
                            currentTag = words[1];
                            tagCategory = TagCategory.SideEffect;
                            i++;
                        }
                    }
                    line = line.substring(currentTag.length());
                    tagContent = new StringBuilder();
                    for(; i < words.length; i++) {
                        tagContent.append(" ");
                        tagContent.append(words[i]);
                    }
                }
            } else {
                // add to open tag
                tagContent.append(" "+line);
            }
        }
        addTag(tagCategory, currentTag, tagContent.toString().trim());
    }

    /** Get the value of a given tag, if specified */
    public String getTagText(String key) {
        if(miscTagDictionary == null) {
            processTags();
        }
        return miscTagDictionary.get(key);
    }

    /** Get the description of a given parameter, if specified */
    public String getParamDesc(String name) {
        if(paramDictionary == null) {
            processTags();
        }
        return paramDictionary.get(name);
    }

    /** Get the justification for a given meta-predicate, if excuse specified */
    public String getJustification(String pred) {
        if(justificationDictionary == null) {
            processTags();
        }
        return justificationDictionary.get(pred);
    }

    /** Get the description for the given side effect */
    public String getSideEffect(String effect) {
        if(sideEffectDictionary == null) {
            processTags();
        }
        return sideEffectDictionary.get(effect);
    }

    public Iterator<Map.Entry<String,String>> iterator() {
        if(miscTagDictionary == null) {
            processTags();
        }
        return miscTagDictionary.entrySet().iterator();
    }

    /** Get all parameters in the right order */
    public List<String> getParameters() {
        if(paramList == null) {
            processTags();
        }
        return paramList;
    }

    /** Get an iterable for the meta-predicate justifications */
    public Iterable<Map.Entry<String,String>> getJustifications() {
        if(justificationDictionary == null) {
            processTags();
        }
        return justificationDictionary.entrySet();
    }

    /** Get an iterable for the side effect descriptions */
    public Iterable<Map.Entry<String,String>> getSideEffects() {
        if(sideEffectDictionary == null) {
            processTags();
        }
        return sideEffectDictionary.entrySet();
    }

    /** Get the description text */
    public String getDescription() {
        if(descriptionText == null) {
            processTags();
        }
        return descriptionText;
    }
    
}
