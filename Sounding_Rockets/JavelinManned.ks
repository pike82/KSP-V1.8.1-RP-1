// Get Mission Values
local wndw is gui(300).
set wndw:x to 400. //window start position
set wndw:y to 120.
local label is wndw:ADDLABEL("Enter Values").
set label:STYLE:ALIGN TO "CENTER".
set label:STYLE:HSTRETCH TO True. // Fill horizontally
local box_alt is wndw:addhlayout().
	local alt_label is box_alt:addlabel("End altitude (km)").
	local destvalue is box_alt:ADDTEXTFIELD("100").
	set destvalue:style:width to 100.
	set destvalue:style:height to 18.
local box_azi is wndw:addhlayout().
	local azi_label is box_azi:addlabel("Heading").
	local azivalue is box_azi:ADDTEXTFIELD("80").
	set azivalue:style:width to 100.
	set azivalue:style:height to 18.
local box_pitch is wndw:addhlayout().
	local pitch_label is box_pitch:addlabel("Start Pitch").
	local pitchvalue is box_pitch:ADDTEXTFIELD("85").
	set pitchvalue:style:width to 100.
	set pitchvalue:style:height to 18.
local somebutton is wndw:addbutton("Confirm").
set somebutton:onclick to Continue@.
// Show the GUI.
wndw:SHOW().
LOCAL isDone IS FALSE.
UNTIL isDone {
	WAIT 1.
}
Function Continue {
		set val to destvalue:text.
		set val to val:tonumber(0).
		set destheight to val*1000.
		set val to azivalue:text.
		set val to val:tonumber(0).
		set sv_intAzimith to val.
		set val to pitchvalue:text.
		set val to val:tonumber(0).
		set sv_anglePitchover to val.
	wndw:hide().
  	set isDone to true.
}
Print "Will dest at: " + destheight + "m".
Print "Start Heading: " + sv_intAzimith.
Print "Start Pitch: " + sv_anglePitchover.
Local sv_ClearanceHeight is 30. //tower clearance height
//Prelaunch
Wait 1. //Alow Variables to be set and Stabilise pre launch
PRINT "Prelaunch.".
set Throttle to 1.
SET SHIP:CONTROL:PILOTMAINTHROTTLE TO 1.
set STEERING TO r(up:pitch,up:yaw,facing:roll).
//Liftoff
STAGE. //Ignite main engines
Print "Starting engines".
Local EngineStartTime is TIME:SECONDS.
Local MaxEngineThrust is 0. 
Local englist is List().
List Engines.
LIST ENGINES IN engList.
FOR eng IN engList {
	Print "eng:STAGE:" + eng:STAGE.
	Print STAGE:NUMBER.
	IF eng:STAGE >= STAGE:NUMBER { 
		SET MaxEngineThrust TO MaxEngineThrust + eng:MAXTHRUST. 
		Print "Stage Full Engine Thrust:" + MaxEngineThrust. 
	}
}
Print "Checking thrust ok".
Local CurrEngineThrust is 0.
Local EngineStartFalied is False.
until CurrEngineThrust > 0.99*MaxEngineThrust{ 
	Set CurrEngineThrust to 0.
	FOR eng IN engList { 
		IF eng:STAGE >= STAGE:NUMBER {
			SET CurrEngineThrust TO CurrEngineThrust + eng:THRUST.
		}
	}
	if (TIME:SECONDS - EngineStartTime) > 5 {
		Lock Throttle to 0.
		Set SHIP:CONTROL:PILOTMAINTHROTTLE TO 0.
		Print "Engine Start up Failed...Making Safe".
		Shutdown. //ends the script
	}
}
Print "Releasing Clamps".
Wait until Stage:Ready .
STAGE. // Relase Clamps
PRINT "Lift off!!".
local LchAlt is ALT:RADAR.
Local holdload is false. 
until holdload = true {
	ff_CheckAbortLow().
		wait 0.2.

		// if (SHIP:Q > 0.35) and (ship:airspeed > 5) { //max Q abort test
		// 	Print "Max Q abort".
		// 	ff_Abort().
		// }
		
		// if (ship:AVAILABLETHRUST/(ship:mass*9.8)) > 5 { //high G abort test
		// 	Print "High G abort".
		// 	ff_Abort().
		// }


	// Clear tower
	if (ALT:RADAR > sv_ClearanceHeight + LchAlt) and (SHIP:Q > 0.05) {
		set STEERING TO HEADING(sv_intAzimith, sv_anglePitchover).
	}
	If (TIME:SECONDS - EngineStartTime) > 20{
		Set holdload to true.
	}
}

Set holdload to false. 
until holdload = true {
	ff_CheckAbortAOA().
		wait 0.2.

	If ship:apoapsis > destheight{
		Set Throttle to 0.
		Set SHIP:CONTROL:PILOTMAINTHROTTLE TO 0.
		Print "Cleared height.".
		Set holdload to true.
	}
}
ff_Para_activate().
wait 30.
Stage.



Function ff_Para_activate{
	for RealChute in ship:modulesNamed("RealChuteModule") {
		RealChute:doevent("arm parachute").
		Print "Parchute armed enabled.".
	}
}

function ff_CheckAbortLow{
	If (verticalspeed < 0) and (altitude < 500) and (ship:airspeed > 5){
		Print"Low Airspeed and altitude abort".
		ff_Abort().
	}
	Local englist is List().
	FOR eng IN engList { 
		If eng:IGNITION and (ship:airspeed > 5){
			if eng:THRUST < 0.95 * eng:AVAILABLETHRUST{
				Print"Engine Failure abort".
				ff_Abort().
			}
		}
	}
}
function ff_CheckAbortAOA{
	If (SHIP:Q > 0.15) and (ship:airspeed > 200) {
		if vang(SHIP:FACING:FOREVECTOR, srfprograde:vector) > 10{
			Print"AoA abort".
			ff_Abort().
		}
	}
	Local englist is List().
	FOR eng IN engList { 
		If eng:IGNITION and (ship:airspeed > 5){
			if eng:THRUST < 0.95 * eng:AVAILABLETHRUST{
				Print"Engine Failure abort".
				ff_Abort().
			}
		}
	}
}

Function ff_Abort {
	Print "Engine Shutdown!!!".
	lock throttle to 0.
	lock PILOTMAINTHROTTLE to 0.
	Local englist is List().
	FOR eng IN engList { 
		If eng:IGNITION {
			eng:shutdown.
		}
	}
	wait 0.1.
	Abort on.
	until (verticalspeed < 1){
		wait 1.
	}
	brakes on.
	ff_Para_activate().//back up for no brakes set but will stop kos
}