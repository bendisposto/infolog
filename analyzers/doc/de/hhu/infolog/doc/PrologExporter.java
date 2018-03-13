package de.hhu.infolog.doc;

import java.io.FileWriter;
import java.io.Writer;
import java.io.IOException;
import java.util.Map;

/** This class exports the gathered coverage information, side effect
 * documentation and meta-predicate justifications to a Prolog file */

public class PrologExporter {

    public void export(String fileName) {
        try {
            FileWriter fw = new FileWriter(fileName);
            fw.write(builtinEffects);
            boolean isDocumented = false, hasMetaPredExcuse = false;
            for(IndexStore.PredicateEntry pred : IndexStore.get().getSortedPredicates(new IndexStore.PredicatesByName())) {
                JoinedComment jc = pred.getPredicate().getComment();
                if(jc != null) {
                    for(Map.Entry<String,String> sideEffect : jc.getSideEffects()) {
                        writeSideEffect(pred, sideEffect.getKey(), sideEffect.getValue(), fw);
                    }
                    for(Map.Entry<String,String> justification : jc.getJustifications()) {
                        writeMetapredExcuse(pred, justification.getKey(), justification.getValue(), fw);
                        hasMetaPredExcuse = true;
                    }
                    writeIsDocumented(pred,fw);
                    isDocumented = true;
                }
            }
            if(!isDocumented) {
                fw.write("is_documented(0).\n");
            }
            if(!hasMetaPredExcuse) {
                fw.write("has_metapred_excuse(0,0,0).\n");
            }
            fw.close();
        } catch(IOException e) {
            e.printStackTrace();
        }
    }

    private void writeSideEffect(IndexStore.PredicateEntry pred, String sideEffect, String description, Writer w) throws IOException {
        w.write("has_documented_sideeffect(");
        w.write(pred.getMaskedModulePredicateArity());
        w.write(", ");
        w.write(sideEffect);
        w.write(", '");
        w.write(description.replace("\\","\\\\").replace("'","\\'"));
        w.write("').\n");
    }

    private void writeMetapredExcuse(IndexStore.PredicateEntry pred, String metaPred, String excuse, Writer w) throws IOException {
        w.write("has_metapred_excuse(");
        w.write(pred.getMaskedModulePredicateArity());
        w.write(", ");
        w.write(metaPred);
        w.write(", '");
        w.write(excuse.replace("\\","\\\\").replace("'","\\'"));
        w.write("').\n");
    }

    private void writeIsDocumented(IndexStore.PredicateEntry pred, Writer w) throws IOException {
        w.write("is_documented(");
        w.write(pred.getMaskedModulePredicateArity());
        w.write(").\n");
    }

    private static String builtinEffects =
        "has_documented_sideeffect(built_in:print/1, print, 'Prints a text to the console').\n"
        + "has_documented_sideeffect(built_in:nl/0, print, 'Prints a line feed to the console').\n"
        + "has_documented_sideeffect(built_in:format/2, print, 'Prints formatted text to the console').\n"
        + "has_documented_sideeffect(built_in:format/3, print, 'Prints formatted text to a stream').\n"
        + "has_documented_sideeffect(built_in:open/3, open, 'Opens a file').\n"
        + "has_documented_sideeffect(built_in:close/1, close, 'Closes a file').\n"
        + "has_documented_sideeffect(built_in:assert/1, assert, 'Adds a fact to the database').\n"
        + "has_documented_sideeffect(built_in:asserta/1, assert, 'Adds a fact to the database').\n"
        + "has_documented_sideeffect(built_in:assertz/1, assert, 'Adds a fact to the database').\n"
        + "has_documented_sideeffect(built_in:retract/1, retract, 'Removes a fact from the database').\n"
        + "has_documented_sideeffect(built_in:retractall/1, retract, 'Removes matching facts from the database').\n";

    
}
