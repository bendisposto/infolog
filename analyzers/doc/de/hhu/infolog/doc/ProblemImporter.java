package de.hhu.infolog.doc;

import java.io.FileReader;
import java.io.BufferedReader;
import java.io.IOException;
import java.util.List;
import java.util.ArrayList;

/** Imports problems from infolog_problems.csv */

public class ProblemImporter {

    public void fromCsv(String fileName) {
        try {
            FileReader fr = new FileReader(fileName);
            BufferedReader br = new BufferedReader(fr);
            String line;
            StringBuilder field = new StringBuilder();
            char c;
            int idx;
            boolean inquote = false;
            List<String> fields = new ArrayList<>();

            while((line = br.readLine()) != null) {
                fields.clear();
                field = new StringBuilder();

                for(idx = 0; idx < line.length(); idx ++) {
                    c = line.charAt(idx);
                    switch(c) {
                    case '"':
                        inquote = !inquote;
                        break;
                    case ',':
                        if(inquote) {
                            field.append(c);
                            break;
                        } else {
                            fields.add(field.toString());
                            field = new StringBuilder();
                        }
                        break;
                    default:
                        field.append(c);
                        break;
                    }
                }

                fields.add(field.toString());
                commit(fields);
                fields.clear();
            }
        } catch(IOException e) {
            e.printStackTrace();
        }
    }

    private void commit(List<String> fields) {
        String severity = fields.get(1),
            problemType = fields.get(0),
            message = fields.get(2),
            moduleName = fields.get(3),
            predName = fields.get(4);
        IndexStore.ProblemEntry pe = new IndexStore.ProblemEntry(problemType, severity, message);
        if(predName.equals("unknown")) {
            IndexStore.get().addModuleProblem(moduleName, pe);
        } else {
            IndexStore.get().addPredicateProblem(moduleName, predName, pe);
        }
    }
    
}
