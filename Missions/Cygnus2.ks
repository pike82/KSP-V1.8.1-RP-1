// Get Mission Values

local wndw is gui(300).
set wndw:x to 700. //window start position
set wndw:y to 120.

Global boosterCPU is "Aethon".

Print "Locking resources".
Local RSS_partlist is list().
Local partlist is List().
Local resourcelist is list().
LIST Parts IN partList. 
FOR Part IN partList {  
	IF (Part:tag >= "Cygnus2") { 
		RSS_partlist:add(Part).
	}
}
For part in RSS_partlist{
	For res in part:Resources{
			Set res:enabled to False.
	}
}

Local holdload is false. 
until holdload = true {
	Set holdload to true. //reset to true and rely on previous stage to turn false
	local PROCESSOR_List is list().
	LIST PROCESSORS IN PROCESSOR_List. // get a list of all connected cores
	//Print PROCESSOR_List:length.
	for Processor in PROCESSOR_List {
		if Processor:TAG = boosterCPU{ //checks to see if previous stage is present
			Set holdload to false.
		}
	}
	wait 2.
}

Print "Cygnus 2 active".
wait 20.
Print "Turn on RCS to commence De-orbit".
Lock steering to retrograde.
Until RCS {
	Wait 2.
}
Print "Moving to Retrograde position".
wait 60.
SET SHIP:CONTROL:FORE to 0.9.
Lock Throttle to 1.
wait 5.
Until ship:orbit:periapsis < 30000{
	Wait 1.
}
until ALT:RADAR < 135000{
	Wait 2.
}
Print "Unlocking resources".
For part in RSS_partlist{
	For res in part:Resources{
			Set res:enabled to True.
	}
}
Stage.//relase rcs engine and camera
wait 5.
Lock Steering to retrograde.
Stage.//Stage chute
Lock Throttle to 0.
Set SHIP:CONTROL:PILOTMAINTHROTTLE TO 0.
until ALT:RADAR < 5000{
	Wait 2.
}
for RealChute in ship:modulesNamed("RealChuteModule") {
	RealChute:doevent("arm parachute").
	Print "Parchute armed enabled.".
}
Wait 60.