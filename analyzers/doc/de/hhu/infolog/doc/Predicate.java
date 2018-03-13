package de.hhu.infolog.doc;

/** This class represents a predicate, possibly with a documentation comment */

public class Predicate {

    private String predName;
    private int arity;
    private JoinedComment comment;

    public Predicate(String predName, int arity,
                     JoinedComment comment) {
        this.predName = predName;
        this.arity = arity;
        this.comment = comment;
    }

    /** Get the predicate name */
    public String getPredicateName() {
        return predName;
    }

    /** Get the predicate arity */
    public int getArity() {
        return arity;
    }
    
    /** Get the documentation comment? */
    public JoinedComment getComment() {
        return comment;
    }

    /** Has this predicate even got a documentation comment? */
    public boolean hasComment() {
        return comment != null;
    }

    public String toString() {
        StringBuilder accum = new StringBuilder();
        if(hasComment()) {
            accum.append(" ** ");
            accum.append(comment.getCommentText());
            accum.append("\n");
        }
        accum.append(predName);
        accum.append("/");
        accum.append(arity);
        return accum.toString();
    }
}
