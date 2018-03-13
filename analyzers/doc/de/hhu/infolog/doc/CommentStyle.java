package de.hhu.infolog.doc;

/** Different styles of commenting */

public enum CommentStyle {
    /** a single %-comment */
    SingleLine,
    /** consecutive %-comments */
    MultiLine,
    /** consecutive %-comments, first one is %% */
    Hook,
    /** normal C-style block comments */
    NormalBlock,
    /** block comments, but each consecutive line begins with an asterisk */
    StarredBlock,
    /** like in JavaDoc, the comment begins with /** */
    JavaDoc
}
