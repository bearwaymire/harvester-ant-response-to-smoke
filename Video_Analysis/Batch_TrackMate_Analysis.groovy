
import ij.IJ
import ij.ImagePlus
import ij.gui.Roi
import ij.gui.GenericDialog
import ij.io.DirectoryChooser
import ij.measure.ResultsTable

import fiji.plugin.trackmate.io.TmXmlReader
import fiji.plugin.trackmate.Model
import fiji.plugin.trackmate.TrackMate

import ij.plugin.HyperStackConverter

import java.io.File

IJ.showMessage("Batch Spot Counter",
    "Choose:\n\n" +
    "1. The FOLDER containing the JPEG image sequence.\n" +
    "2. The TrackMate XML file with saved paramters.\n" +
    "3. The MASK (.tif) that defines the mound area.\n\n" +
    "Then enter up to 8 start frames and the window length (default 100).\n\n" +
    "Results are shown per 100-frame window in a Results Table."
)

def chooser = new DirectoryChooser("Select image sequence folder")
def dir = chooser.getDirectory()
if (dir == null) return
def optDir = "open=[" + dir + "]"

def xmlPath = IJ.getFilePath("Select your TrackMate XML settings file")
if (xmlPath == null) return
def xmlFile = new File(xmlPath)

def maskPath = IJ.getFilePath("Select a saved BINARY mask image (.tif)")
if (maskPath == null) { IJ.error("Mask is required."); return }
def maskImp = IJ.openImage(maskPath)
IJ.run(maskImp, "Create Selection", "")
def bufferedROI = maskImp.getRoi()
maskImp.close()
if (bufferedROI == null) { IJ.error("Could not extract ROI from mask."); return }

def gd = new GenericDialog("Batch TrackMate")
gd.addMessage("Enter up to 8 start frames (leave blanks if unused):")
for (int i=1; i<=8; i++) gd.addStringField("Start "+i+":", "")
gd.addNumericField("Window length (frames):", 100, 0)
gd.showDialog()
if (gd.wasCanceled()) return

def starts = []
for (int i=0; i<8; i++) {
    def s = gd.getNextString().trim()
    if (s) {
        try { starts << Integer.parseInt(s) } catch (Exception e) { IJ.log("Skipping invalid start: " + s) }
    }
}
int winLen = (int) gd.getNextNumber()
if (starts.isEmpty()) { IJ.error("No valid start frames entered."); return }

def rt = new ResultsTable()

for (int startFrame : starts) {
    String opts = "${optDir} number=${winLen} starting=${startFrame} increment=1 sort"
    IJ.run("Image Sequence...", opts)
    def imp = IJ.getImage()
    if (imp == null) { IJ.log("Could not open window starting at " + startFrame); continue }

    imp.setRoi((Roi) bufferedROI.clone())
    IJ.run(imp, "Clear Outside", "stack")
    IJ.run(imp, "Crop", "stack")
    IJ.run(imp, "8-bit", "")
    IJ.run(imp, "Invert", "stack")
    IJ.run(imp, "Enlarge...", "enlarge=-15")

    imp = HyperStackConverter.toHyperStack(imp, 1, 1, imp.getNSlices(), "default")
    imp.setOpenAsHyperStack(true)

    def reader   = new TmXmlReader(xmlFile)
    def settings = reader.readSettings(imp)
    def model    = new Model()
    def tm       = new TrackMate(model, settings)

    println(imp.getNChannels())
    println(imp.getNSlices())
    println(imp.getNFrames())

    if (!tm.execDetection()) {
        IJ.log("Detection failed at start " + startFrame + ": " + tm.getErrorMessage())
        imp.changes = false
        imp.close()
        continue
    }

    def filters = null
    try {
        filters = settings.getSpotFilters()
    } catch (Throwable t) {
        IJ.log("Warning: could not read spot filters from settings; counting all spots.")
    }

    def spots = model.getSpots()

    def tempFrames = spots.keySet().toList().sort()
    println(tempFrames)
    tempFrames.each { f ->
        int nAll = spots.getNSpots(f, false)
        int nVis = spots.getNSpots(f, true)
        println "frame=$f  total=$nAll visible=$nVis"
    }
    int kept = 0

    def it = spots.iterator(false)

    while (it.hasNext()) {
        def spot = it.next()
        boolean ok = true

        if (filters != null) {
            for (ff in filters) {
                def feat
                def thr
                boolean above = true
                try { feat = ff.feature } catch (Throwable t) { try { feat = ff.getFeature() } catch (Throwable t2) { feat = null } }
                try { thr  = ff.value   } catch (Throwable t) { try { thr  = ff.getValue()   } catch (Throwable t2) { thr = null } }
                try { above = ff.isAbove } catch (Throwable t) { try { above = ff.isAbove() } catch (Throwable t2) { above = true } }

                if (feat == null || thr == null) { ok = false; break }
                def val = spot.getFeature(feat)
                if (val == null) { ok = false; break }

                double v = val.doubleValue()
                double th = (thr instanceof Number) ? thr.doubleValue() : Double.parseDouble(thr.toString())

                if (above) {
                    if (v < th) { ok = false; break }
                } else {
                    if (v > th) { ok = false; break }
                }
            }
        }

        if (ok) kept++
    }

    rt.incrementCounter()
    rt.addValue("StartFrame", startFrame)
    rt.addValue("EndFrame", startFrame + winLen - 1)
    rt.addValue("TotalSpots", kept)

    imp.changes = false
    imp.close()
}

rt.show("Batch Spot Counts")
IJ.log("Done. Processed " + starts.size() + " window(s).")
