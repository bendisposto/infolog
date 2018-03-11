package de.hhu.infolog.doc;

import java.util.List;
import java.util.LinkedList;
import java.util.Iterator;
import java.util.Map;
import java.util.Hashtable;

/** This class merges clause heads with their bodies and 
 *  documentation comments */

public class Joiner implements Iterable<Predicate> {

    private Parser source;
    private List<Predicate> sink;
    private Map<Parser.PredicateArity,Predicate> map;
    private JoinedComment moduleComment;

    public Joiner(Parser source) {
        this.source = source;
        this.sink = null;
        this.map = null;
        this.moduleComment = null;
    }

    /** Merge clause heads with their bodies and documentation comments */
    public void join() {
        sink = new LinkedList<Predicate>();
        JoinedComment comment = null;
        List<LineCommentChunk> lineComments = new LinkedList<LineCommentChunk>();
        BlockCommentChunk blockComment = null;
        String predName = "";
        int arity = 0;
        boolean wasModule = source.isModule();
        ClauseHeadChunk openClause = null;

        try {
            for(Chunk chunk : source) {
                switch(chunk.getChunkType()) {
                case LineComment:
                    blockComment = null;
                    lineComments.add((LineCommentChunk)chunk);
                    break;
                case BlockComment:
                    lineComments.clear();
                    blockComment = (BlockCommentChunk)chunk;
                    break;
                case ClauseHead:
                    openClause = (ClauseHeadChunk) chunk;
                    if(!openClause.hasBody() &&
                       !(openClause.getPredicateName().equals(predName) &&
                         openClause.getArity() == arity)) {
                        // emit current predicate
                        if(!predName.equals("")) {
                            sink.add(new Predicate(predName,arity,comment));
                        }
                        arity = openClause.getArity();
                        predName = openClause.getPredicateName();
                        comment = chooseComment(lineComments,blockComment);
                    }
                    if(!openClause.hasBody()) {
                        openClause = null;
                        lineComments.clear();
                        blockComment = null;
                    }
                    break;
                case ClauseBody:
                    if(openClause != null) {
                        ClauseBodyChunk body = (ClauseBodyChunk) chunk;
                        int realArity = openClause.getArity() +
                            (body.isDCG() ? 2 : 0);
                        if(!openClause.getPredicateName().equals(predName) ||
                           realArity != arity) {
                            // emit current predicate
                            if(!predName.equals("")) {
                                sink.add(new Predicate(predName,arity,comment));
                            }
                            arity = realArity;
                            predName = openClause.getPredicateName();
                            comment = chooseComment(lineComments,blockComment);
                        }
                        lineComments.clear();
                        blockComment = null;
                        openClause = null;
                    } else {
                        if(!wasModule && source.isModule()) {
                            moduleComment = chooseComment(lineComments,blockComment);
                        }
                        lineComments.clear();
                        blockComment = null;
                    }
                    break;
                }
                wasModule = source.isModule();
            }
        } catch(Parser.ParseException e) {
            // print the message and recover
            // usually we can safely continue
            e.printStackTrace();
        }
        // emit leftover predicate
        if(!predName.equals("")) {
            sink.add(new Predicate(predName,arity,comment));
        }
        // add predicates to map
        map = new Hashtable<Parser.PredicateArity,Predicate>();
        for(Predicate p : sink) {
            map.put(new Parser.PredicateArity(p.getPredicateName(),
                                              p.getArity()), p);
        }
    }

    /** Join line comments or use block comment, depending on which one is available */
    private JoinedComment chooseComment(List<LineCommentChunk> lineComments, BlockCommentChunk blockComment) {
        if(blockComment != null) {
            return new JoinedComment(blockComment);
        } else if(!lineComments.isEmpty()) {
            return new JoinedComment(lineComments);
        } else {
            return null;
        }
    }

    public Iterator<Predicate> iterator() {
        if(sink == null) join();
        return sink.iterator();
    }

    /** Look up a predicate from the hash map */
    public Predicate lookup(String predName, int arity) {
        return lookup(new Parser.PredicateArity(predName, arity));
    }

    /** Look up a predicate from the hash map */
    public Predicate lookup(Parser.PredicateArity pa) {
        if(map == null) join();
        return map.get(pa);
    }

    /** Get the module comment (if there is one) */
    public JoinedComment getModuleComment() {
        return moduleComment;
    }
}
