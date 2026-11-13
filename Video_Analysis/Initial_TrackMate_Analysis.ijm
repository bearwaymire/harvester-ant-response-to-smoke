waitForUser("Message", "When you are prompted to open image sequence, import a sequence short enough (preferably less than 100 images)");

run("Image Sequence...");
//waitForUser("Message", "File -> Import -> image sequence, preferably less than 100 images. Once done, press OK");


Dialog.create("Mask question");
items = newArray("Select new area", "Load existing area");
Dialog.addRadioButtonGroup("Action", items, 1, 2, "Select new area");
Dialog.show;
userChoice = Dialog.getRadioButton();

setForegroundColor(0, 0, 0);
setBackgroundColor(0, 0, 0);

if(userChoice == "Select new area")
{
	setTool("polygon");
	waitForUser("Message", "Select mound area. Once done, press OK");
	
	run("Clear Outside", "stack");
	run("Select None");
	setTool("Paintbrush Tool");
	waitForUser("Message", "Paint over any obstacles and press OK");
	run("Duplicate...", "title=mask");
	run("8-bit");
	setThreshold(1, 255);
	setOption("BlackBackground", true);
	run("Convert to Mask");
	//waitForUser("what?");
	run("Select None");
	run("Create Selection");
	//waitForUser("what?2");
	run("Create Mask");
	//waitForUser("what?3");
	run("Save");
	run("Close");
	run("Select None");
	run("Close");
	run("Restore Selection");
	
	
}

if(userChoice == "Load existing area")
{
	run("Open...");
	// run("Invert");  // REMOVED to avoid flipping inside/outside
	run("Create Selection");
	//waitForUser("what4");
	run("Select None");
	run("Close");
	run("Restore Selection");
}

run("Clear Outside", "stack");
run("Crop", "stack");
run("8-bit");
run("Invert", "stack");
run("Enlarge...", "enlarge=-15");
run("TrackMate");
waitForUser("Message", "Click next in trackmate until you can adjust filters");
