// Test: Call graph lookup bug - constructor edge incorrectly matched
// The presence of a constructor creates an edge in the call graph.
// Without the fix, this edge was incorrectly matched when looking up
// the signature for newDocumentBuilder(), causing the taint to be lost.
//
// Without fix (main): 0 findings
// With fix: 1 finding

package net.sourceforge.pmd;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import org.w3c.dom.Document;
import java.io.OutputStream;

public class RuleSetWriter {
    private final OutputStream outputStream;
    private Document document;

    // This constructor is key - it creates an edge that confuses the lookup
    public RuleSetWriter(OutputStream outputStream) {
        this.outputStream = outputStream;
    }

    public void write() {
        try {
            DocumentBuilderFactory documentBuilderFactory = DocumentBuilderFactory.newInstance();
            // ruleid: xxe-call-graph-fix
            DocumentBuilder documentBuilder = documentBuilderFactory.newDocumentBuilder();
            document = documentBuilder.newDocument();
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }
}
