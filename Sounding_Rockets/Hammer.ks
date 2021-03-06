// Get Booster Values
local wndw is gui(300).
set wndw:x to 400. //window start position
set wndw:y to 120.
local label is wndw:ADDLABEL("Enter Values").
  	set label:STYLE:ALIGN TO "CENTER".
  	set label:STYLE:HSTRETCH TO True. // Fill horizontally
local box_azi is wndw:addhlayout().
  	local azi_label is box_azi:addlabel("Heading").
  	local azivalue is box_azi:ADDTEXTFIELD("90").
  	set azivalue:style:width to 100.
  	set azivalue:style:height to 18.
local box_pitch is wndw:addhlayout().
  	local pitch_label is box_pitch:addlabel("Start Pitch").
  	local pitchvalue is box_pitch:ADDTEXTFIELD("89").
  	set pitchvalue:style:width to 100.
  	set pitchvalue:style:height to 18.
local box_LVL is wndw:addhlayout().
	local LVL_label is box_LVL:addlabel("Level angle").
	local LVLvalue is box_LVL:ADDTEXTFIELD("-7").
	set LVLvalue:style:width to 100.
	set LVLvalue:style:height to 18.
local somebutton is wndw:addbutton("Confirm").
set somebutton:onclick to Continue@.

// Show the GUI.
wndw:SHOW().
LOCAL isDone IS FALSE.
UNTIL isDone {
	WAIT 1.
}
Function Continue {
set val to azivalue:text.
	set val to val:tonumber(0).
	set sv_intAzimith to val.
set val to pitchvalue:text.
	set val to val:tonumber(0).
	set sv_anglePitchover to val.
set val to LVLvalue:text.
	set val to val:tonumber(0).
	set pitchdown to val.
	wndw:hide().
  	set isDone to true.
}
Print "Start Heading: " + sv_intAzimith.
Print "Start Pitch: " + sv_anglePitchover. 
Print "Pitching at: " + pitchdown.
Local sv_ClearanceHeight is 30. //tower clearance height
//Prelaunch
Wait 1. 
PRINT "Prelaunch.".
Lock Throttle to 1.
SET SHIP:CONTROL:PILOTMAINTHROTTLE TO 1.
LOCK STEERING TO r(up:pitch,up:yaw,facing:roll).
//Liftoff
STAGE. //main engines
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
STAGE. //Clamps
PRINT "Lift off!!".
local LchAlt is ALT:RADAR.
// Clear tower
Wait UNTIL ALT:RADAR > sv_ClearanceHeight + LchAlt.
Wait UNTIL SHIP:Q > 0.015. 
// Pitchover
LOCK STEERING TO HEADING((sv_intAzimith-0), sv_anglePitchover).
Print "Pitchover: " + (TIME:SECONDS - EngineStartTime).
Wait 10.
// Gravity Turn
LOCK STEERING TO SRFPROGRADE.
Print "Gravity Turn: " + (TIME:SECONDS - EngineStartTime).
wait 30.
Until SHIP:Q < 0.05{
	Wait 0.2.
}
Print "Fairings: " + (TIME:SECONDS - EngineStartTime).
Stage.
Wait 1.
// unguided
until (TIME:SECONDS - EngineStartTime) > 140{
	wait 0.5.
}
LOCK STEERING TO HEADING(sv_intAzimith, pitchdown).
until (TIME:SECONDS - EngineStartTime) > 155{
	wait 0.5.
}
Print "Spin Stabilisation: " + (TIME:SECONDS - EngineStartTime).
unlock steering.
SAS on.
wait 0.25.
set ship:control:roll to 1.
Until AVAILABLETHRUST < 1{
	Wait 0.1.
}
Print "MECO: " + (TIME:SECONDS - EngineStartTime).
Stage.//First stage and ullage
Wait until Stage:Ready.
Print (TIME:SECONDS - EngineStartTime).
Print "Apo: " + ship:apoapsis.
//Stage.//ullage
Wait until Stage:Ready.
Stage.//engine
Wait 80.
Lock Throttle to 0.
Set SHIP:CONTROL:PILOTMAINTHROTTLE TO 0.
Wait 1.0.
Shutdown.
