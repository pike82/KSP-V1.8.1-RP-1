Local sv_ClearanceHeight is 10. 
//Prelaunch
Wait 1. 
PRINT "Prelaunch.".
Lock Throttle to 1.
SET SHIP:CONTROL:PILOTMAINTHROTTLE TO 1.
LOCK STEERING TO r(up:pitch,up:yaw,facing:roll). 
//Liftoff
STAGE. //Ignite main engines
Print "Starting engines".
Local EngineStartTime is TIME:SECONDS.
Local MaxEngThrust is 0. 
Local englist is List().
List Engines.
LIST ENGINES IN engList. 
FOR eng IN engList { 
	Print "eng:STAGE:" + eng:STAGE.
	Print STAGE:NUMBER.
	IF eng:STAGE >= STAGE:NUMBER { 
		SET MaxEngThrust TO MaxEngThrust + eng:MAXTHRUST. 
		Print "Stage Full Engine Thrust:" + MaxEngThrust. 
	}
}
Print "Checking thrust".
Local CurrEngineThrust is 0.
Local EngineStartFalied is False.
until CurrEngineThrust > 0.99*MaxEngThrust{ 
	Set CurrEngineThrust to 0.
	FOR eng IN engList {  
		IF eng:STAGE >= STAGE:NUMBER { 
			SET CurrEngineThrust TO CurrEngineThrust + eng:THRUST. 
		}
	}
	if (TIME:SECONDS - EngineStartTime) > 5 {
		Lock Throttle to 0.
		Set SHIP:CONTROL:PILOTMAINTHROTTLE TO 0.
		Print "Engine Start Failed...Making Safe".
		Shutdown. //ends the script
	}
}
wait 0.2.
Print "Releasing Clamps".
Wait until Stage:Ready . 
STAGE. // Relase Clamps
PRINT "Lift off!!".
Wait until Stage:Ready. 
wait 0.5.
Stage.//drop solid
Unlock Steering. 
Print ship:altitude.
Print ship:verticalspeed.
Until ((ship:verticalspeed < 0) and (ship:altitude > 200)){
	wait 2.	
}
Stage.//parachute activate if installed
wait 1.
for RealChute in ship:modulesNamed("RealChuteModule") {
	RealChute:doevent("arm parachute").
	Print "Parchute armed enabled.".
}