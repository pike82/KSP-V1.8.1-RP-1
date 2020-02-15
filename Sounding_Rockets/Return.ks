// Get Mission Values
local wndw is gui(300).
set wndw:x to 400. //window start position
set wndw:y to 120.
Print "Return program intiated".
Set SHIP:CONTROL:PILOTMAINTHROTTLE TO 0.
until (ship:verticalspeed > 50){
	Wait 5.
}
until (ship:verticalspeed < 0) and (ALT:RADAR < 5000){
	Wait 5.
}
Stage.//get rid of nose cone
wait 5.
for RealChute in ship:modulesNamed("RealChuteModule") {
	RealChute:doevent("arm parachute").
	Print "Parchute armed enabled.".
}