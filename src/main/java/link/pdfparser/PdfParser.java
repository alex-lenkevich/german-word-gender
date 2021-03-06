
/*
    This file is part of the iText (R) project.
    Copyright (c) 1998-2020 iText Group NV
    Authors: iText Software.

    For more information, please contact iText Software at this address:
    sales@itextpdf.com
 */
/**
 * Example written by Bruno Lowagie in answer to:
 * http://stackoverflow.com/questions/24506830/can-we-use-text-extraction-strategy-after-applying-location-extraction-strategy
 */
package link.pdfparser;

import com.itextpdf.kernel.font.PdfFont;
import com.itextpdf.kernel.geom.Rectangle;
import com.itextpdf.kernel.pdf.PdfDocument;
import com.itextpdf.kernel.pdf.PdfReader;
import com.itextpdf.kernel.pdf.canvas.parser.EventType;
import com.itextpdf.kernel.pdf.canvas.parser.PdfCanvasProcessor;
import com.itextpdf.kernel.pdf.canvas.parser.data.IEventData;
import com.itextpdf.kernel.pdf.canvas.parser.data.TextRenderInfo;
import com.itextpdf.kernel.pdf.canvas.parser.filter.IEventFilter;
import com.itextpdf.kernel.pdf.canvas.parser.filter.TextRegionEventFilter;
import com.itextpdf.kernel.pdf.canvas.parser.listener.FilteredEventListener;
import com.itextpdf.kernel.pdf.canvas.parser.listener.IEventListener;
import com.itextpdf.kernel.pdf.canvas.parser.listener.LocationTextExtractionStrategy;
import com.itextpdf.kernel.pdf.canvas.parser.listener.SimpleTextExtractionStrategy;

import java.io.File;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.io.FileOutputStream;
import java.io.Writer;
import java.io.BufferedWriter;
import java.util.Set;

import static com.itextpdf.kernel.pdf.canvas.parser.EventType.*;

public class PdfParser {
    public static final String DEST = "./Gnetzwerk_a1-b1_unregelmaessigeverben.txt";

    public static final String SRC = "/Users/oleksandr.linkevych/Downloads/netzwerk_a1-b1_unregelmaessigeverben (1).pdf";

    public static void main(String[] args) throws IOException {
        new PdfParser().manipulatePdf(DEST);
    }

    protected void manipulatePdf(String dest) throws IOException {
        PdfDocument pdfDoc = new PdfDocument(new PdfReader(SRC));

//        pdfDoc.getPage(10)

//        CustomAlignFilter fontFilter = new CustomAlignFilter();
        FilteredEventListener listener = new FilteredEventListener();

        // Create a text extraction renderer
        PdfEventListener extractionStrategy = listener
                .attachEventListener(new PdfEventListener()/*, fontFilter*/);

        // Note: If you want to re-use the PdfCanvasProcessor, you must call PdfCanvasProcessor.reset()
        PdfCanvasProcessor parser = new PdfCanvasProcessor(listener);
        for (int i = 1; i <= pdfDoc.getNumberOfPages(); i++) {
            parser.processPageContent(pdfDoc.getPage(i));
        }


        // Get the resultant text after applying the custom filter
//        String actualText = extractionStrategy.getResultantText();

//        System.out.println(actualText);

        pdfDoc.close();

        // See the resultant text in the console
//        System.out.println(actualText);

//        try (Writer writer = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(dest)))) {
//            writer.write(actualText);
//        }
    }

    class PdfEventListener implements IEventListener {

        @Override
        public void eventOccurred(IEventData data, EventType type) {
            TextRenderInfo textRenderInfo = (TextRenderInfo) data;
            System.out.println(textRenderInfo.getText() + " - " + textRenderInfo.getBaseline().getBoundingRectangle().getY());
        }

        @Override
        public Set<EventType> getSupportedEvents() {
            return Set.of(RENDER_TEXT);
        }
    }


}
