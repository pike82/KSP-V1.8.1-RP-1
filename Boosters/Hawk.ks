// Get Booster Values
Print core:tag.
local wndw is gui(300).
set wndw:x to 400. //window start position
set wndw:y to 20.

local label is wndw:ADDLABEL("Enter Booster Values").
set label:STYLE:ALIGN TO "CENTER".
set label:STYLE:HSTRETCH TO True. // Fill horizontally

local box_LAN is wndw:addhlayout().
	local LAN_label is box_LAN:addlabel("Desired LAN").
	local LANvalue is box_LAN:ADDTEXTFIELD("0").
	set LANvalue:style:width to 100.
	set LANvalue:style:height to 18.

local box_inc is wndw:addhlayout().
	local inc_label is box_inc:addlabel("Desired Inclination").
	local incvalue is box_inc:ADDTEXTFIELD("0").
	set incvalue:style:width to 100.
	set incvalue:style:height to 18.

local box_node is wndw:addhlayout().
	local node_label is box_node:addlabel("Towards AN").
	local nodevalue is box_node:ADDTEXTFIELD("True").
	set nodevalue:style:width to 100.
	set nodevalue:style:height to 18.

local box_pitch is wndw:addhlayout().
	local pitch_label is box_pitch:addlabel("Start Pitch").
	local pitchvalue is box_pitch:ADDTEXTFIELD("84").
	set pitchvalue:style:width to 100.
	set pitchvalue:style:height to 18.

local box_APalt is wndw:addhlayout().
	local APalt_label is box_APalt:addlabel("End AP(km)").
	local APaltvalue is box_APalt:ADDTEXTFIELD("190").
	set APaltvalue:style:width to 100.
	set APaltvalue:style:height to 18.

local box_PEalt is wndw:addhlayout().
	local PEalt_label is box_PEalt:addlabel("End PE(km)").
	local PEaltvalue is box_PEalt:ADDTEXTFIELD("190").
	set PEaltvalue:style:width to 100.
	set PEaltvalue:style:height to 18.

local box_TAR is wndw:addhlayout().
	local TAR_label is box_TAR:addlabel("Launch Target").
	local TARvalue is box_TAR:ADDTEXTFIELD("Earth").
	set TARvalue:style:width to 100.
	set TARvalue:style:height to 18.

local box_OFF is wndw:addhlayout().
	local OFF_label is box_OFF:addlabel("Avg time to orbit (s)").
	local OFFvalue is box_OFF:ADDTEXTFIELD("300").
	set OFFvalue:style:width to 100.
	set OFFvalue:style:height to 18.

local box_Stg is wndw:addhlayout().
	local Stg_label is box_Stg:addlabel("Stages").
	local Stgvalue is box_Stg:ADDTEXTFIELD("2").
	set Stgvalue:style:width to 100.
	set Stgvalue:style:height to 18.

local somebutton is wndw:addbutton("Confirm").
set somebutton:onclick to Continue@.

// Show the GUI.
wndw:SHOW().
LOCAL isDone IS FALSE.
UNTIL isDone {
	WAIT 1.
}

Function Continue {
		set val to LANvalue:text.
		set val to val:tonumber(0).
		set tgt_LAN to val.

		set val to incvalue:text.
		set val to val:tonumber(0).
		Global tgt_inc is val.

		set val to nodevalue:text.
		set To_AN to val.

		set val to pitchvalue:text.
		set val to val:tonumber(0).
		set sv_anglePitchover to val.

		set val to APaltvalue:text.
		set val to val:tonumber(0).
		Global tgt_ap is val*1000.

		set val to PEaltvalue:text.
		set val to val:tonumber(0).
		Global tgt_pe is val*1000.

		set val to TARvalue:text.
		set val to body(val).
		set L_TAR to val.

		set val to OFFvalue:text.
		set val to val:tonumber(0).
		set L_OFF to val.

		set val to Stgvalue:text.
		set val to val:tonumber(0).
		set Stg to val.

	wndw:hide().
  	set isDone to true.
}

Print "Taget LAN: " + tgt_LAN.
Print "Inc: " + tgt_inc.
Print "to_an: " + to_an.
Print "Start Pitch: " + sv_anglePitchover. 
Print "AP at: " + tgt_ap + "m".
Print "PE turn at: " + tgt_pe + "m". 
Print "Target: " + L_TAR.
Print "Offset: " + L_OFF.
Print ship:GEOPOSITION:lat.

// Mission Values

local lncwin is ff_launchwindow(tgt_inc, L_TAR, tgt_LAN, To_AN).//returns azimuth and time until launch
Print lncwin [0].
Print lncwin [1].
Global sv_intAzimith is lncwin [0]. 
Local EngineStartTime is lncwin [1].
Set EngineStartTime to TIME:SECONDS + EngineStartTime - (L_OFF).
Local sv_ClearanceHeight is 150. //tower clearance height


//Prelaunch

warpto (EngineStartTime -5).
PRINT "Prelaunch.".
Lock Throttle to 1.

SET SHIP:CONTROL:PILOTMAINTHROTTLE TO 1.
LOCK STEERING TO r(up:pitch,up:yaw,facing:roll). 

//Liftoff
Wait until TIME:SECONDS > (EngineStartTime).
STAGE. //Ignite main engines
Print "Starting engines".
Set EngineStartTime to TIME:SECONDS.
Local MaxEngineThrust is 0. 
Local englist is List().
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
		//Print eng:name.
		IF eng:STAGE >= STAGE:NUMBER { 
			SET CurrEngineThrust TO CurrEngineThrust + eng:THRUST. 
		}
	}
	if (TIME:SECONDS - EngineStartTime) > 10 {
		Lock Throttle to 0.
		Set SHIP:CONTROL:PILOTMAINTHROTTLE TO 0.
		Print "Engine Start up Failed...Making Safe".
		Shutdown. //ends the script
	}
}
Print "Releasing Clamps".
Wait until Stage:Ready.
STAGE. // Relase Clamps
Global liftoff is time:seconds.
PRINT "Lift off!!".
local LchAlt is ALT:RADAR.

// Clear tower

Wait UNTIL ALT:RADAR > sv_ClearanceHeight + LchAlt.
LOCK STEERING TO HEADING(sv_intAzimith, 90).
Wait UNTIL SHIP:Q > 0.02. 
LOCK STEERING TO HEADING((sv_intAzimith-0), sv_anglePitchover).
Print "Pitchover: " + (TIME:SECONDS - EngineStartTime).
Wait 10.
lock pitch to 90 - VANG(SHIP:UP:VECTOR, SHIP:VELOCITY:SURFACE).
LOCK STEERING TO heading(sv_intAzimith, pitch) .
Print "Gravity Turn: " + (TIME:SECONDS - EngineStartTime).

RCS on.
local staging is false.
until staging{
	if AVAILABLETHRUST < 1 {
		Stage. // Decouple the stage
		Wait 0.3.
		Wait until Stage:Ready.
		Set staging to true.
		STAGE. //Start the next engine
		wait 1.
	}
	wait 1.
} 

f_Orbit_Steer(Stg).

Print "Ascent finished".
Lock Throttle to 0.
Set SHIP:CONTROL:PILOTMAINTHROTTLE TO 0.
wait 1.0.
Print "Orbit Reached".
// Auto Change the name tag of the probe.
Set CORE:Part:Tag to "Nil".
Shutdown. //ends the script


function ff_launchwindow{
Parameter tgt_inc is 0, tgt is Earth, tgt_LAN is 0, To_AN is true.
	local RetAzi is 90.
	local RetETA is 0.

//Work out Earth possibilities
	If tgt = Earth{
		If (tgt_inc = 0) and (tgt_LAN = 0) {
			Return list(RetAzi, RetETA).// launch east now
		} 
		
		If (tgt_inc = 0){
			Set RetETA to ff_ETAToPlane(BODY, tgt_LAN, 90, To_AN).
			Return list(RetAzi, RetETA).// launch Azi at eta

		}


		set ra to body:radius + tgt_ap. //full Ap
		set rp to body:radius + tgt_pe. //full pe
		local sma is (ra+rp)/2. //sma
		local ecc is (ra-rp)/(ra+rp). //eccentricity
		local V_p is sqrt((2*body:mu*ra)/(rp*2*sma)). // this is the target velocity at the periapsis

		if tgt_inc < abs(latitude) { //If the inclination of the target is less than the lattitude of the ship
			Set tgt_inc to abs(latitude).
			Print "Latitude unable to allow normal Launch to inclination, switching to nearest inclination!!!".
			wait 5.
			Set RetAzi to f_FlightAzimuth(tgt_inc, V_p).
			If To_AN <> true { Set RetAzi to 180 - RetAzi.}

			If tgt_LAN = 0{
				Return list(RetAzi, RetETA).// launch Azi now
			}
		}

		Set RetAzi to f_FlightAzimuth(tgt_inc, V_p).
		If tgt_LAN = 0{
				Return list(RetAzi, RetETA).// launch Azi now
		}
		Set RetETA to ff_ETAToPlane(BODY, tgt_LAN, tgt_inc, To_AN).
		Return list(RetAzi, RetETA).// launch Azi at eta
	}

	Set tgt_inc to tgt:orbit:inclination.// for craft and the moon and other planets
	Set tgt_LAN to tgt:OBT:LAN. // for craft and the moon and other planets
//Work out solar system possibilities

	if tgt_inc < abs(latitude) { //If the inclination of the target is less than the lattitude of the ship
		Set tgt_inc to abs(latitude).
		Set incDiff to ship:orbit:inclination-tgt:orbit:inclination.
		Print "Latitude unable to allow normal Launch to inclination, switching to nearest inclination!!!".
		wait 5.
	}

	Print "tgt_inc" + tgt_inc.
	Print "tgt_LAN" + tgt_LAN.

	set ra to body:radius + tgt_ap. //full Ap
	set rp to body:radius + tgt_pe. //full pe
	local sma is (ra+rp)/2. //sma
	local ecc is (ra-rp)/(ra+rp). //eccentricity
	local V_p is sqrt((2*body:mu*ra)/(rp*2*sma)). // this is the target velocity at the periapsis

	Set RetAzi to f_FlightAzimuth(tgt_inc, V_p).
	Set RetETA to ff_ETAToPlane(BODY, tgt_LAN, tgt_inc, To_AN).
	Return list(RetAzi, RetETA).// launch Azi at eta

	Print vang(hf_normalvector(ship),hf_normalvector(tgt)).
	wait 5.
	return vang(hf_normalvector(ship),hf_normalvector(tgt)).
}

function f_FlightAzimuth {
	parameter inc, V_orb. // target inclination

	// project desired orbit onto surface heading
	local az_orb is arcsin ( cos(inc) / cos(ship:latitude)).
	if (inc < 0) {
		set az_orb to 180 - az_orb.
	}
	
	// create desired orbit velocity vector
	local V_star is heading(az_orb, 0)*v(0, 0, V_orb).

	// find horizontal component of current orbital velocity vector
	local V_ship_h is ship:velocity:orbit - vdot(ship:velocity:orbit, up:vector)*up:vector.
	
	// calculate difference between desired orbital vector and current (this is the direction we go)
	local V_corr is V_star - V_ship_h.
	
	// project the velocity correction vector onto north and east directions
	local vel_n is vdot(V_corr, ship:north:vector).
	local vel_e is vdot(V_corr, heading(90,0):vector).
	
	// calculate compass heading
	local az_corr is arctan2(vel_e, vel_n).
	return az_corr.

}// End of Function

function ff_ETAToPlane {
	PARAMETER tgt, orb_lan, 
  	i, is_AN is True, 
	  ship_lat is ship:latitude, 
	  ship_lng is Ship:Longitude.//South to North if ascending_node is TRUE, North to South if it is FALSE
	
	Print ship_lat.
	Print i.
	Print TAN(ship_lat).
	Print TAN(i).

	LOCAL rel_lng IS ARCSIN(TAN(ship_lat)/TAN(i)).
    IF NOT is_AN { SET rel_lng TO 180 - rel_lng. }
    LOCAL g_lan IS hf_mAngle(orb_lan + rel_lng - tgt:ROTATIONANGLE).
    LOCAL node_angle IS hf_mAngle(g_lan - ship_lng).
	SET r_eta TO (node_angle / 360) * tgt:ROTATIONPERIOD.
	//Print "ETA " + r_eta.
  	RETURN r_eta.
}

function hf_normalvector{
	parameter ves.
	Local vel is velocityat(ves,time:seconds):orbit.
	Local norm is vcrs(vel,ves:up:vector). 
	return norm:normalized.// gives vector pointing towards centre of body from ship
}


function f_Orbit_Steer{
//Used for second stage taking into account the third "end" stage
/////////////////////////////////////////////////////////////////////////////////////
// Credits: Own modifications to:
// http://www.orbiterwiki.org/wiki/Powered_Explicit_Guidance
//With Large assisstance and corrections from:
// https://github.com/Noiredd/PEGAS
// https://ntrs.nasa.gov/archive/nasa/casi.ntrs.nasa.gov/19660006073.pdf
// https://amyparent.com/post/automating-rocket-launches/

	Parameter 
	Stages,

	//tgt_pe is 191100, //target periapsis
	//tgt_ap is 191100, //target apoapsis
	//tgt_inc is 26.8, //target inclination

	u is 0,//Target true anomoly
	HSL is 5, //end shutdown margin

	T3 is 30, // stage three estimated burn length
	mass_flow3 is 16.2, //estimated mass flow
	start_mass3 is 9463, //estimated start mass in kg
	s_Ve3 is 4135, //estimated exhuast vel (thrust/massflow)
	tau3 is 590, //(S-Ve/avg_acc) effective time to burn all propellant

	//T2_end is 360, //stage two estimated burn ending time from liftoff
	T2 is 227, // stage two estimated burn length
	mass_flow2 is 102, //estimated mass flow
	start_mass2 is 34860, //estimated start mass in kg
	s_Ve2 is 3064, //estimated exhuast vel (thrust/massflow)
	tau2 is 220, //(S-Ve/avg_acc) estimated effective time to burn all propellant

	//T1_end is 0, //stage one estimated burn ending time from liftoff
	T1 is 0, // stage one estimated burn length
	mass_flow1 is 0, //estimated mass flow
	s_Ve1 is 1, //estimated exhuast vel (do not make 0)
	tau1 is 1. //specific impulse???

	If Stages < 2{
		Set T2 to 0.
	}
	If Stages < 3{
		Set T1 to 0.
	}

	local Thrust2 is s_Ve2 * mass_flow2.
	local Thrust3 is s_Ve3 * mass_flow3.

//starting peg variables
    local A1 is -0.3.
    local B1 is 0. 
    local C1 is 0.1. 

    local A2 is -0.15. 
    local B2 is 0. 
    local C2 is 0.1.

	local A3 is 0. 
    local B3 is 0. 
    local C3 is 0.1. 

    local converged is 1. // used by convergence checker
    local delta is 0. //time between peg loops
	local peg_step is 1.0.//time between each calcuation check
	local A is 0.
	local B is 0.
	local C is 0.
	local s_pitch is 20.

	local dA1 is 0.
	local dA2 is 0.
	local dB1 is 0.
	local dB2 is 0.

	//values setup
    set ra to body:radius + tgt_ap. //full Ap
    set rp to body:radius + tgt_pe. //full pe
    local sma is (ra+rp)/2. //sma
    local ecc is (ra-rp)/(ra+rp). //eccentricity
    local vp is sqrt((2*body:mu*ra)/(rp*2*sma)). // this is the target velocity at the periapsis
	if u = 0 { //u=0 means PE is target position in orbit
    	set tgt_vy to 0. // this is the split of the target velocity at the point in time
    	set tgt_vx to vp. // this is the split of the target velocity at the point in time (should be zero for u = 0)
		set rc to rp. // this is the target radius based on the desire true anomoly
	}else{
		set rc to (sma*(1-ecc^2))/(1+ecc*cos(u)). // this is the target radius based on the desire true anomoly
    	local vc is sqrt((vp^2) + 2*body:mu*((1/rc)-(1/rp))). // this is the target velocity at the target radius (if u is zero this will equal vp)
    	local uc is 90 - arcsin((rp*vp)/(rc*vc)). // this is the direction vector of the target velocity
    	set tgt_vy to vc*sin(uc). // this is the split of the target velocity at the point in time
    	set tgt_vx to vc*cos(uc). // this is the split of the target velocity at the point in time (should be zero for u = 0)
	}

    // Define target position and velocities

	local tgt_r is rc.
    Local tgt_h is vcrs(v(tgt_r, 0, 0), v(tgt_vy, tgt_vx, 0)):mag. // target angular momentum. This is the velocity represented as energy at a point made up of the x and y components.
	Local tgt_w is sqrt((tgt_vx^2) + (tgt_vy^2)) / (tgt_r).

	Local I is 0.
	Local fairlock is false.
	local tau_lock is false.

	Print "PEG Values set up".
	Print ship:mass.
	Print ship:drymass.
	SET STEERINGMANAGER:MAXSTOPPINGTIME TO 1.//can adjust for strenth/speed of changes to steeering control


    local rTcur is (time:seconds - liftoff).
	local last is (time:seconds - liftoff).
	local lastM is (time:seconds - liftoff).
    local s_r is ship:orbit:body:distance.
	local s_acc is ship:AVAILABLETHRUST/ship:mass.
	local s_vy is ship:verticalspeed.
	local s_vx is sqrt(ship:velocity:orbit:sqrmagnitude - ship:verticalspeed^2).
	local w is s_vx / s_r.
	local s_ve is f_Vel_Exhaust().
	local tau is s_ve/s_acc. //time to burn ship if all propellant
	local w_T1 is w*1.01. //first guess at tgt_w which is actually the current w plus 1%.
	local rT1 is s_r + ((tgt_r-s_r)*0.75). //first guess at rT1
	local w_T2 is w_T1*1.01. 
	local rT2 is s_r + ((tgt_r-s_r)*0.9). //first guess at rT2
	local w_T3 is w_T2*1.01. 
	local rT3 is tgt_r. //first guess at rT3
	local hT1 is tgt_h *0.98.
	local hT2 is tgt_h *0.99.
	local hT3 is tgt_h.

	Clearscreen.
	local loop_break to false.
	//Loop through updating the parameters until the break condition is met
    until false {

		if (SHIP:Q < 0.005) and (fairlock = false) {
			f_rel_Fairing().
			Print "Fairings Delpolyed: " + (TIME:SECONDS - EngineStartTime).
			set fairlock to true.
		}

		//Collect updated time periods
        set rTcur to (time:seconds - liftoff).
		Set DeltaM to rTcur - LastM. // time since last minor calc loop
		set delta to rTcur - last. // time since last major calculation
		set Last to rTcur.
		set A to A + (B*DeltaM).

		// collect current ship parameters
        set s_r to ship:orbit:body:distance.
		set s_acc to ship:AVAILABLETHRUST/ship:mass.
		set s_vy to ship:verticalspeed.
		set s_vx to sqrt(ship:velocity:orbit:sqrmagnitude - ship:verticalspeed^2).
		set w to s_vx / s_r.
		Local h is vcrs(v(s_r, 0, 0), v(s_vy, s_vx, 0)):mag. 

		If tau_lock = false{
			set s_ve to f_Vel_Exhaust().
			Set tau to s_ve/s_acc.
		} else{
			Set tau to 300.
		}


		if T1> 0 and (tau_lock = false){
			Print "IGM Phase 1" .//AT (0,1).
			Set T1 to T1 - DeltaM.
			//Set T1 to T1-delta. //found this is not accurate enough to track T1 count down
			Set A1 to A1 + (B1 * DeltaM).
			if T1 < 3 { 
				Set tau_lock to true.
				Print"tau locked".
			}Else{
				set s_Ve1 to s_ve.
				set tau1 to tau.
			}
		}

		if (T2> 0)  and (T1 = 0) and (tau_lock = false){
			Print "IGM Phase 2" .//AT (0,1).
			Set T2 to T2 - DeltaM.
			//Set T2 to T2-delta. /found this is not accurate enough to track T2 count down
			Set A2 to A2 + (B2 * DeltaM).
			if T2 < 3 {
				Set tau_lock to true.
				Print"tau locked".
			}Else{
				set s_Ve2 to s_ve.
				set tau2 to tau.
			}
		}

		if T3> 0 and (T2 = 0) and (tau_lock = false){
			Print "IGM Phase 3" .//AT (0,1).
			Set T3 to T3 - DeltaM.
			Set A3 to A3 + (B3 * DeltaM).
			set s_Ve3 to s_ve.
			set tau3 to tau.
		}
		Print "AVAILABLETHRUST:" +AVAILABLETHRUST.
		if (AVAILABLETHRUST < 5) {
			Stage.// release
			wait 0.1.
			Wait until Stage:Ready . 
			If T1=0{
				Set T2 to 0.
			} Else {
				Set T1 to 0.
			}
			//Set T2 to 0. //end phase 2
			Print "Staging".
			Stage.// start next engine
			wait 3.
			Set tau_lock to false.
			set s_acc to ship:AVAILABLETHRUST/ship:mass.//needs to be reset fro remainder of loop
			//SET STEERINGMANAGER:MAXSTOPPINGTIME TO 1.
		}

		//cutoff process
		if  ((HSL - delta) < 2) and (tau_lock = true) and (T2 = 0){
			Until false{
				set s_vx to sqrt(ship:velocity:orbit:sqrmagnitude - ship:verticalspeed^2).
				if (tgt_vx -10) < s_vx{
					lock Throttle to 0.
					RCS on.
					SET SHIP:CONTROL:FORE TO 1.0.
					until (ship:orbit:eccentricity < 0.0001) or (ship:periapsis > tgt_pe) or (tgt_vx < s_vx){
						wait 0.01.
						set s_vx to sqrt(ship:velocity:orbit:sqrmagnitude - ship:verticalspeed^2).
					}
					SET SHIP:CONTROL:FORE TO 0.0.
					RCS off.
					Print "Insertion: "+(TIME:SECONDS).
					Set loop_break to true.
					break.
				}
				Print tgt_vx.
				Print s_vx.
				wait 0.001.
			}
		}
		
		//////////PEG Minor loop//////////////////////
    	If (delta >= peg_step) and (tau_lock = false){  // this is used to ensure a minimum time step occurs before undertaking the next peg cycle calculations
			Set last to (time:seconds - liftoff).//reset last
			/// determine peg states
			local peg_solved is f_PEG(A1, B1, T1, rT1, hT1, w_T1, s_ve1, tau1, tgt_vy, tgt_vx, tgt_r, tgt_w, mass_flow1,  
										A2, B2, T2, rT2, hT2, w_T2, s_ve2, tau2, start_mass2, mass_flow2, Thrust2,
										A3, B3, T3, rT3, hT3, w_T3, s_ve3, tau3, start_mass3, mass_flow3, Thrust3,
										dA1, dA2, dB1, dB2).

			set A1 to peg_solved[0].
			set B1 to peg_solved[1].
			set T1_new to peg_solved[2].
			set rT1 to peg_solved[3].
			set hT1 to peg_solved[4].
			set w_T1_new to peg_solved[5].

			set A2 to peg_solved[6].
			set B2 to peg_solved[7].
			set T2_new to peg_solved[8].
			set rT2 to peg_solved[9].
			set hT2 to peg_solved[10].
			set w_T2_new to peg_solved[11].

			set A3 to peg_solved[12].
			set B3 to peg_solved[13].
			set T3_new to peg_solved[14].
			set rT3 to peg_solved[15].
			set hT3 to peg_solved[16].
			set w_T3_new to peg_solved[17].

			set dA1 to peg_solved[18].
			set dA2 to peg_solved[19].
			set dB1 to peg_solved[20].
			set dB2 to peg_solved[21].

	
			if T1 >0 {

				set w_T1 to w_T1_new.
				set w_T2 to w_T2_new.
				set w_T3 to w_T3_new.
				set T3 to T3_new.
				set A to A1.
				set B to B1.
			} 

			if (T2> 0) and (T1 = 0){
				set w_T2 to w_T2_new.
				set w_T3 to w_T3_new.
				set T3 to T3_new.
				set A to A2.
				set B to B2.
			}
			if T3> 0 and (T2 = 0){

				if(T3_new <= HSL) { // below this the solution starts to become very sensitive and A and B should not longer be re-calculated but fixed until insertion
					Print "Terminal guidance enabled". 
					set converged to -5.
					Set peg_step to 1000.
					Print tau_lock.
					Set tau_lock to true.
				} Else{
					Print "Non terminal guidance".
					set A to A3.
					set B to B3.
				}
				Set T3 to T3_new.
				set w_T3 to w_T3_new.

			}			
		}


		//Print A.
		//Print B.
		//Print C.
		//Print w.
		//Print s_acc.
		//Print peg_step.
		If loop_break = true {
			Break.// exit loop
		}
		set C to ((body:mu/(s_r^2)) - ((w^2)*s_r))/s_acc.	
		set s_pitch to A + C. //sin pitch at current time.
		set s_pitch to max(-0.707, min(s_pitch, 0.707)). // limit the pitch change to between -45 and 45 degress
		Set s_pitch to arcsin(s_pitch). //covert into degress
		//if tgt_inc = 0{
			LOCK STEERING TO heading(sv_intAzimith, s_pitch).
		//}Else{
		//	if (converged = 1){
		//		local fixed_head is f_FlightAzimuth(tgt_inc, tgt_vx).
		//		LOCK STEERING TO heading(fixed_head, s_pitch).
		//	}

		//}
		Print "S pitch: " + s_pitch.
		Print (HSL - delta).
	wait 0.1.
	}//end of loop

} // end of function
	

function f_PEG {
    parameter A1.
    parameter B1.
    parameter T1.
	parameter rT1.
	parameter hT1.
	parameter w_T1. 
	parameter s_ve1.
	parameter tau1.
	parameter tgt_vy. // orbit "target" vertical velocity
	parameter tgt_vx. // orbit "target" horizontal velocity
	parameter tgt_r. // orbit "target" radius
	parameter tgt_w.
	parameter mass_flow1 is 0.

    parameter A2 is 0.
    parameter B2 is 0.
    parameter T2 is 0.
	parameter rT2 is 0.
	parameter hT2 is 0.
	parameter w_T2 is 0. 
	parameter s_ve2 is 0.
	parameter tau2 is 1.
	parameter start_mass2 is 0.
	parameter mass_flow2 is 0.
	parameter Thrust2 is 0.

	parameter A3 is 0.
    parameter B3 is 0.
    parameter T3 is 0.
	parameter rT3 is 0.
	parameter hT3 is 0.
	parameter w_T3 is 0. 
	parameter s_ve3 is 0.
	parameter tau3 is 1.
	parameter start_mass3 is 0.
	parameter mass_flow3 is 0.
	parameter Thrust3 is 0.

	parameter dA1 is 0.
	parameter dA2 is 0.
	parameter dB1 is 0.
	parameter dB2 is 0.

	// read current stage and position values

	local s_vy is ship:verticalspeed.
	local s_vx is sqrt(ship:velocity:orbit:sqrmagnitude - ship:verticalspeed^2).
	local s_r is ship:orbit:body:distance.
	local s_acc is ship:AVAILABLETHRUST/ship:mass. // current ship parameter
	local w is s_vx /s_r.
	//local w is sqrt((s_vx^2) + (s_vy^2)) / (s_r).
	local h0 is vcrs(v(s_r, 0, 0), v(s_vy, s_vx, 0)):mag. //current angular momentum
	Local tgt_h is vcrs(v(tgt_r, 0, 0), v(tgt_vy, tgt_vx, 0)):mag. //target angular momentum

	local s_acc_2 is 0.
	local s_acc_3 is 0.

	local s_acc_end_1 is 0.
	local s_acc_end_2 is 0.
	local s_acc_end_3 is 0.

	local rdotT2 is 0.
	local rdotT3 is 0.
	local A is 0.
	local B is 0.


	if T1 > 0{
		Set A to A1.
		Set B to B1.
	}
	if (T2 > 0) and (T1 = 0){
		Set A to A2.
		Set B to B2.
		Set rT1 to s_r.
	}
	if (T2 = 0) and (T1 = 0){
		Set A to A3.
		Set B to B3.
		Set rT2 to s_r.
	}

	/// determine bn and cn stages

	local L1 is f_bcn(s_ve1, tau1, T1).

	local bb01 is L1[0].
	local bb11 is L1[1].
	local bb21 is L1[2].
	local cc01 is L1[3].
	local cc11 is L1[4].
	local cc21 is L1[5].

	local L2 is f_bcn(s_ve2, tau2, T2).

	local bb02 is L2[0].
	local bb12 is L2[1].
	local bb22 is L2[2].
	local cc02 is L2[3].
	local cc12 is L2[4].
	local cc22 is L2[5].

	local L3 is f_bcn(s_ve3, tau3, T3).

	local bb03 is L3[0].
	local bb13 is L3[1].
	local bb23 is L3[2].
	local cc03 is L3[3].
	local cc13 is L3[4].
	local cc23 is L3[5].

	Print "Checks check:".
	Print "bb01: " + bb01.
	Print "bb11: " + bb11.
	Print "s_r: "+s_r.
	Print "s_vy: " + s_vy.
	Print "tgt_r: "+tgt_r.
	Print "T1: " + T1.
	Print "A1: " + A1.
	Print "B1: " + B1.
	Print "rT1: " + rT1.
	Print "dA1: " + dA1.
	Print "dB1: " + dB1.
	Print "T2: " + T2.
	Print "A2: " + A2.
	Print "B2: " + B2.
	Print "rT2: " + rT2.
	Print "dA2: " + dA2.
	Print "dB2: " + dB2.
	Print "T3: " + T3.
	Print "A3: " + A3.
	Print "B3: " + B3.
	Print "rT3: " + rT3.
	Print "Height: "+ (rT1 - body:radius).
	// Print "(s_vy*T1)" + (s_vy*T1).
	// Print "(cc01 * A1)" + (cc01 * A1).
	// Print "(cc11*B1)" + (cc11*B1).
	// Print "combined: " + ((s_vy*T1)+(cc01 * A1)+(cc11*B1)).


	//get future stage parameters
	//T3 parameters
	set s_acc_3 to Thrust3/start_mass3.
	set s_acc_end_3 to Thrust3/ (start_mass3 -((mass_flow3)*T3)).

	//J= 4 l=3, k=2, i=1
	set rdotT3 to s_vy.
	set rdotT3 to rdotT3 + (bb03+bb02+bb01)*A.
	set rdotT3 to rdotT3 + ( (bb13 + (bb03*(T2+T1))) + (bb12 + (bb02*T1)) + (bb11 + (bb01*0)) )*B.   //vertical speed at staging
	set rdotT3 to rdotT3 + ((bb03*dA2) + (bb03*(T2)*dB1) + (bb13*dB2)) + ((bb02*dA1) + (bb02*T1*0) + (bb12*dB1)) + ((bb01*0) + (bb01*0*0) + (bb11*0)).
	Print "Calc rdotT3 check " + rdotT3.
	set rdotT3 to tgt_vy.
	
	//J=4 l=3, k=2, i=1, m=0
	set rT3 to s_r + (s_vy*(T1+T2+T3)). 
	set rT3 to rT3 + ( (cc03 + (T3*(bb01+bb02))) + (cc02 + (T2*bb01)) + (cc01 + (T1*0)) )*A.
	set rT3 to rT3 + ( cc13 + cc12 + cc11 + (cc03*T2 + bb12*T3 + bb02*T3*T1) + (cc03*T1) + (bb11*T3) + ((bb01*T3)*0) + (cc02*T1 + bb11*T2 + bb01*T2*0) + (cc01*0 + 0*T1 + 0*T1*0)  )*B.
	set rT3 to rT3 + ((cc03*dA2) + (cc13*dB2) + (cc02*dA1) + (cc12*dB1)).
	set rT3 to rT3 + (bb02*T3*dA1 + bb02*T1*T3*0 + bb12*T3*dB1 + cc03*T2*dB1).
	set rT3 to rT3 + (bb01*T3*dA1) + (bb01*T1*T3*0) + (bb11*T3*dB1) + (cc03*T1*dB1).//l=3, k=1, i=1, m=0
	Print "Calc RT3 check " + rT3.
	set rT3 to tgt_r.
	
	Print "rdotT3: "+rdotT3.
	Print "rT3: "+ rT3.

	//apply boundaries on results
	//if rT3 > tgt_r{ Set rT3 to tgt_r.}
	//if rT3 < rT2 {Set rT3 to rT2.}

	Local L6 is f_end_cond(w_T2, rT2, s_acc_3, w_T3, rT3, s_acc_end_3, T3, A3, B3). 
	local ft_3 is L6[0].
	local ftdot_3 is L6[1].
	local ftdd_3 is L6[2].
	//local dh_T3 to ((rT2 + rT3)/2)*( (ft_3*bb03) + (ftdot_3*bb13) + (ftdd_2*bb23) ).
	//Set hT3 to dh_T3 + hT2.
	local dh_T3 is tgt_h - hT2. //angular momentum to gain in final stage
	//Set hT3 to dh_T3 + hT2.
	Local hT3 is tgt_h.
	//local v0_T3 is hT3/rT3.
	local v0_T3 is tgt_vx.
	//Print L6.
	//print "v0_T3 " + v0_T3.
	local rT3 is tgt_r.
	//Print "rT3" + rT3.
	//Set w_T3 to sqrt((v0_T3^2) - (rdotT3^2))/rT3.
	Set w_T3 to tgt_w.

	set mean_r to (rT3 + rT2)/2.
	local dv_T3 is dh_T3/mean_r.
	if (dv_T3 < 5) and (T1 > 0) {Set dv_T3 to 5.}
	Set T3 to tau3*(1 - constant:e ^ (-dv_T3/s_ve3)).

	if T3 <0 {Set T3 to 2.}
	Print "dv gain T2 to Orbit: " + dv_T3.

	//T2 parameters
	set s_acc_2 to Thrust2/start_mass2.
	set s_acc_end_2 to Thrust2/ (start_mass2 -((mass_flow2)*T2)).

	//J= 3 l=2, k=1, i=0
	set rdotT2 to s_vy.
	set rdotT2 to rdotT2 + (bb02+bb01)*A.
	set rdotT2 to rdotT2 + ( (bb12 + (bb02*T1)) + (bb11 + (bb01*0)) )*B.   //vertical speed at staging
	set rdotT2 to rdotT2 + ((bb02*dA1) + (bb02*T1*0) + (bb12*dB1)) + ((bb01*0) + (bb01*0*0) + (bb11*0)).
	Print "Calc rdotT2 check " + rdotT2.
	
	//J=3 l=2, k=1, i=0
	set rT2 to s_r + (s_vy*(T1+T2)). 
	set rT2 to rT2 + ( (cc02 + (T2*bb01)) + (cc01 + (T1*0)) )*A.
	set rT2 to rT2 + ( cc12 + cc11 + (cc02*T1 + bb11*T2 + bb01*T2*0) + (cc01*0 + 0*T1 + 0*T1*0)  )*B.
	set rT2 to rT2 + ( (cc02*dA1) + (cc12*dB1) + (cc01*0) + (cc11*0)  ).
	set rT2 to rT2 + (bb01*T2*0 + bb01*0*T2*0 + bb11*T2*0 + cc02*T1*0).
	//Print "Calc RT2 check " + rT2.

	//apply boundaries on results
	//if rT2 > tgt_r{ Set rT2 to tgt_r.}
	//if rT2 < rT1 { Set rT2 to rT1.}

	Local L5 is f_end_cond(w_T1, rT1, s_acc_2, w_T2, rT2, s_acc_end_2, T2, A2, B2). 
	local ft_2 is L5[0].
	local ftdot_2 is L5[1].
	local ftdd_2 is L5[2].
	local dh_T2 to ((rT1 + rT2)/2)*( (ft_2*bb02) + (ftdot_2*bb12) + (ftdd_2*bb22) ).
	Set hT2 to dh_T2 + hT1.
	local v0_T2 is hT2/rT2.
	//contraint on V0_T1 to less than remaining stage dv
	//Print "v0_T2 (1): " + v0_T2.
	// if V0_T2 > (bb02 + V0_T1){
	// 	set V0_T2 to (bb02 + V0_T1).
	// 	set hT2 to V0_T2*rT2.
	// }
	// if V0_T2 < (V0_T1){
	// 	set V0_T2 to (V0_T1).
	// 	set hT2 to V0_T2*rT2.
	// }
	//Print L5.
	//print "v0_T2 " + v0_T2.
	//print "rT2" + rT2.
	Set w_T2 to sqrt((v0_T2^2) - (rdotT2^2))/rT2.

	set mean_r to (rT2 + rT1)/2.
	local dv_gain is dh_T2/mean_r.
	Print "dv gain to T1 to T2: " + dv_gain.

	if T3 = 0{ // if only two stage to orbit
		Set T2 to tau2*(1 - constant:e ^ (-dv_gain/s_ve2)).
		//set T2 boundaries
		if T2 <0 {Set T2 to 2.}
	}

	//T1 parameters
	set s_acc_end_1 to ship:AVAILABLETHRUST/ (ship:mass - ((mass_flow1/1000)*T1)).// 1000 used here as mass returned in tonnes due to weight
	Print s_acc_end_1.

	//J= 2 l=1, k=0, i=0
	set rdotT1 to s_vy.
	set rdotT1 to rdotT1 + (bb01)*A.
	set rdotT1 to rdotT1 + ( (bb11 + (bb01*0)) )*B.   //vertical speed at staging
	set rdotT1 to rdotT1 + ((bb01*0) + (bb01*0*0) + (bb11*0)).
	Print "Calc rdotT1 check " + rdotT1.
	
	//J= 2 l=1, k=0, i=0
	set rT1 to s_r + (s_vy*(T1)). 
	set rT1 to rT1 + ( (cc01 + (T1*0)) )*A.
	set rT1 to rT1 + ( cc11 + (cc01*0 + 0*T1 + 0*T1*0)  )*B.
	set rT1 to rT1 + ( (cc01*0) + (cc11*0) ).
	// Print "Calc RT1 check " + rT1.
	// Print "Change rDot3: " + (bb03*A3 + bb13*B3).
	// Print "Change r3: " + (rdotT2*T3) + cc03*A3 + cc12*B3.
	// Print "Change rDot2: " + (bb02*A2 + bb12*B2).
	// Print "Change r2: " + (rdotT1*T2) + cc02*A2 + cc12*B2.
	// Print "Change rDot1: " + (bb01*A1 + bb11*B1).
	// Print "Change r1: " + (s_vy*T1) + cc01*A1 + cc11*B1.

	//apply boundaries on results
	//if rT1 > tgt_r {Set rT1 to tgt_r-30000.}
	//if rT1 < s_r {Set rT1 to s_r.}

	local L4 is f_end_cond(w, s_r, s_acc, w_T1, rT1, s_acc_end_1, T1, A1, B1). 
	local ft_1 is L4[0].
	local ftdot_1 is L4[1].
	local ftdd_1 is L4[2].
	local dh_T1 to ((s_r + rT1)/2)*( (ft_1*bb01) + (ftdot_1*bb11) + (ftdd_1*bb21) ).
	Set hT1 to dh_T1 + h0.
	local v0_T1 is hT1/rT1.
	//contraint on V0_T1 to less than remaining stage dv
	//Print "v0_T1 (1): " + v0_T1.
	Print dh_T1 /rT1.
	// if V0_T1 > (bb01 + sqrt(s_vx^2 + s_vy^2)){
	// 	set V0_T1 to (bb01 + sqrt(s_vx^2 + s_vy^2)).
	// 	set hT1 to V0_T1*rT1.
	// }
	//Print tau1. 
	//Print "T1: " + T1.
	//Print L1.
	//Print L4.
	//Print "v0_T1: " + v0_T1.
	//Print "rT1: " + rT1.
	Set w_T1 to sqrt((v0_T1^2) - (rdotT1^2))/rT1.

	set mean_r to (s_r + rT1)/2.
	local dv_gain is dh_T1/mean_r.
	Print "dv gain to T1: " + dv_gain.

	if T2 = 0 { // if only single stage to orbit
		Set T1 to tau1*(1 - constant:e ^ (-dv_gain/s_ve1)).
	}

	//Guidance staging discontinuities

	If (T2>0) and (T1>0){

		set dA1 to ( (body:mu/(rT1^2)) - ((w_T1^2)*rT1) ).
		set dA1 to dA1 * ( (1/s_acc_end_1) - (1/s_acc_2) ).

		set dB1 to - ( (body:mu/(rT1^2)) - ((w_T1^2)*rT1) ) * ( (1/s_ve1) - (1/s_ve2)  ). 
		set dB1 to dB1 + ( ( (3*(w_T1^2)) - ((2*body:mu)/(rT1^3)) ) *rdotT1* ( (1/s_acc_end_1) - (1/s_acc_2) )  ).

		/// Determine A2 and B2

		set A2 to A1 + (dA1 + (B1*T1)). //Aj = A1 + sum dA(l) + B1*T(l) + T(l)*sum(dB(l)) (from l=1 to j-1)
		set B2 to B1 + dB1. //Bj = B1 + sum dB(l) (from l=1 to j-1)
		
		If T3>0{

			set dA2 to ( (body:mu/(rT2^2)) - ((w_T2^2)*rT2) ).
			set dA2 to dA2 * ( (1/s_acc_end_2) - (1/s_acc_3) ).

			set dB2 to - ( (body:mu/(rT2^2)) - ((w_T2^2)*rT2) ) * ( (1/s_ve2) - (1/s_ve3)  ). 
			set dB2 to dB2 + ( ( (3*(w_T2^2)) - ((2*body:mu)/(rT2^3)) ) *rdotT2* ( (1/s_acc_end_2) - (1/s_acc_3) )  ).

			/// Determine A3 and B3

			set A3 to A1 + (dA1 + (B1*T1)) + (dA2 + (B1*T2)) + (T2*dB1). //Aj = A1 + sum dA(l) + B1*T(l) + T(l)*sum(dB(l)) (from l=1 to j-1)
			set B3 to B1 + dB1 + dB2. //Bj = B1 + sum dB(l) (from l=1 to j-1)
		
		}
	}

	If (T2>0) and (T1=0){
		Set dA1 to 0.
		Set dB1 to 0.

		set dA2 to ( (body:mu/(rT2^2)) - ((w_T2^2)*rT2) ).
		set dA2 to dA2 * ( (1/s_acc_end_2) - (1/s_acc_3) ).

		set dB2 to - ( (body:mu/(rT2^2)) - ((w_T2^2)*rT2) ) * ( (1/s_ve2) - (1/s_ve3)  ). 
		set dB2 to dB2 + ( ( (3*(w_T2^2)) - ((2*body:mu)/(rT2^3)) ) *rdotT2* ( (1/s_acc_end_2) - (1/s_acc) )  ).

		/// Determine A3 and B3

		set A3 to A2 + (dA2 + (B2*T2)). 
		set B3 to B3 + dB2.
		
	}

	If (T2=0) and (T1=0){
		Set dA1 to 0.
		Set dB1 to 0.

		Set dA2 to 0.
		Set dB2 to 0.
	}

	//set up matricies
	//for rDot A/////
	local mA11 is bb01 + bb02 + bb03. 

	//for rDot B////
	//l=3, k=2
	Local mA12 is bb13 + (bb03*(T1+T2)). 
	//l=2, k=1 
	set mA12 to mA12 + bb12 + (bb02*T1).
	//l=1
	set mA12 to mA12 + bb11.

	//for r A/////
	//l=3, k=2,1
	local mA21 is cc03 + ((bb02+bb01)*T3). 
	//l=2, k=1 
	set mA21 to mA21 + cc02 + (bb01*T2).
	//l=1, k=0 
	set mA21 to mA21 + cc01.

	//for r B/////
	//l=3, k=1 i=1 
	local mA22 is cc13 + (cc03*T2) + (bb12*T3) + ((bb02*T3)*T1).
	// sub l=3, k=1 i=0 
		set mA22 to mA22 + (cc03*T1) + (bb11*T3) + ((bb01*T3)*0).
	//l=2, k=1, i=0 
	set mA22 to mA22 + cc12 + (cc02*T1) + (bb11*T2) +((bb01*T2)*0).
	// sub l=2, k=0 i=0 
		set mA22 to mA22 + (cc02*0) + (0*T2) + ((0*T2)*0).
	//l=1, k=0, i=0 
	set mA22 to mA22 + cc11. 


	//for rdot final/////
	local mC1 is tgt_vy - s_vy. 
	//l=3, k=2 i=1
	set mC1 to mC1 - (bb03*dA2) - (bb03*(T2)*dB1) - (bb13*dB2). 
	//l=2, k=1, i=0
	set mC1 to mC1 - (bb02*dA1) - (bb02*T1*0) - (bb12*dB1). 
	//l=1, k=0, i=0
	set mC1 to mC1 - (bb01*0) - (bb01*0*0) - (bb11*0). 

	
	//for r final/////
	local mC2 is tgt_r - s_r - (s_vy*(T1+T2+T3)).
	//l=3, k=2, i=1, m=0
	set mC2 to mC2 - (cc03*dA2) - (cc13*dB2) - (bb02*T3*dA1) - (bb02*T1*T3*0) - (bb12*T3*dB1) - (cc03*T2*dB1).
	// Sub l=3, k=1, i=1, m=0
		set mC2 to mC2 - (bb01*T3*dA1) - (bb01*T1*T3*0) - (bb11*T3*dB1) - (cc03*T1*dB1).
	// Sub l=3, k=2, i=0, m=0
		set mC2 to mC2 - (bb02*T3*0) - (bb02*0*T3*0) - (bb12*T3*0) - (cc03*T2*0).
	//l=2, k=1, i=0, m=0
	set mC2 to mC2 - (cc02*dA1) - (cc12*dB1) - (bb01 *T2 *0) - (bb01*0*T2*0) - (bb11*T2*0) - (cc02*T1*0). 
	// Sub l=2, k=0, i=0, m=0
		set mC2 to mC2 - (0*T2*0) - (0*0*T2*0) - (0*T2*0) - (cc02*0*0).
	//l=1, k=0, i=0, m=0
	set mC2 to mC2 - (cc01*0) - (cc11*0) - 0. 

	local peg is f_peg_solve(mA11, mA12, mA21, mA22, mC1, mC2).

	if T1 > 0{
		Set A1 to peg[0].
		Set B1 to peg[1].
		Print "A1 peg"+ A1. 
		Print "B1 peg" + B1.
	}
	if (T2 > 0) and (T1 = 0){
		Set A2 to peg[0].
		Set B2 to peg[1].
		Print "A2 peg"+ A2. 
		Print "B2 peg" + B2.
	}
	if (T2 = 0) and (T1 = 0){
		Set A3 to peg[0].
		Set B3 to peg[1].
		Print "A3 peg"+ A3. 
		Print "B3 peg" + B3.
	}
	// Print "Accel " + s_acc.
	// Print s_acc_2.
	// Print s_acc_3.
	// Print s_acc_end_1.
	// Print s_acc_end_2.
	// Print s_acc_end_3.

	Return list(A1, B1,	T1, rT1, hT1, w_T1, A2, B2, T2, rT2, hT2, w_T2, A3, B3, T3, rT3, hT3, w_T3, dA1, dA2, dB1, dB2). 
}

///////////////////////////////////////////////////////////////////////////////////
function f_bcn{
	parameter s_ve.
	parameter tau.
	parameter T.

	local bb0 is -s_ve*(LN(1-(T/tau))).
	//J1
	local bb1 is (bb0 * tau) - (s_ve*T).
	//P1
	local bb2 is (bb1 * tau) - ((s_ve*(T^2))/2).
	//S1
	local cc0 is (bb0*T)-bb1.
	//Q1
	local cc1 is (cc0*tau) - ((s_ve*(T^2))/2).
	//U1
	local cc2 is (cc1*tau) - ((s_ve*(T^3))/6).

	return list(bb0, bb1, bb2, cc0, cc1, cc2).
}
Function f_end_cond{
	parameter start_w.
	parameter start_r.
	parameter start_acc.
	parameter end_w.
	parameter end_r.
	parameter end_acc.
	parameter T_time.
	parameter A.
	parameter B. 

	if T_Time = 0{
		Set T_Time to 1. // prevent divide by zero error.
	}

	//Current pitch guidance for horizontal state
	Set C to ((body:mu/(start_r^2)) - ((start_w^2)*start_r))/start_acc. //start portion of vehicle acceleration used to counteract gravity
	local fr is A + C. //sin pitch at start
	local C_end is (body:mu/(end_r^2)) - ((end_w^2)*end_r). //Gravity and centrifugal force term at cutoff
	Set C_end to C_end /end_acc. 
	Set frT to A + (B*T_time) + C_end. //sin pitch at burnout. 
	local frdot is (frT-fr)/T_time. //approximate rate of sin pitch
	local ft is 1 - (frT^2)/2. //cos pitch
	local ftdot is -fr*frdot. //cos pitch speed
	local ftdd is -(frdot^2)/2. //cos pitch acceleration
	
	return list (ft, ftdot, ftdd). 
}
///////////////////////////////////////////////////////////////////////////////////
// Estimate, returns A and B coefficient for guidance
function f_peg_solve {
    parameter mA11.
	parameter mA12. 
	parameter mA21. 
	parameter mA22. 
	parameter mC1. 
	parameter mC2.

	//solve matrix
	local d is 1/((mA11*mA22) - (mA12*mA21)). // inverse coefficent
	//inverse matrix
	local dmA11 is d*mA22.
	local dmA12 is d*-1*mA12.
	local dmA21 is d*-1*mA21.
	local dmA22 is d*mA11.

	//Multiple inverse matrix by result matrix
	local A is dmA11*mC1 + dmA12*mC2.
	local B is dmA21*mC1 + dmA22*mC2.

	// //solve matrix
	// local d is ((mA11*mA22) - (mA12*mA21)). // inverse coefficent

	// //Multiple inverse matrix by result matrix
	// local A is (mA22*mC1 - mA12*mC2)/d.
	// local B is (mA11*mC2 - mA21*mC1)/d.



    return list(A, B).
}


///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////

function f_Vel_Exhaust {
	local g is 9.80665.  // Gravitational acceleration constant used in game for Isp Calculation (m/s²)
	local engine_count is 0.
	local thrust is 0.
	local isp is 0. // Engine ISP (s)
	list engines in all_engines.
	for en in all_engines if en:ignition and not en:flameout {
	  set thrust to thrust + en:availablethrust.
	  set isp to isp + en:isp.
	  set engine_count to engine_count + 1.
	}
	set isp to isp / engine_count.
	return g *isp.///thrust). //
}/// End Function

function f_mdot {
	local g is 9.80665.  // Gravitational acceleration constant used in game for Isp Calculation (m/s²)
	local engine_count is 0.
	local thrust is 0.
	local isp is 0. // Engine ISP (s)
	list engines in all_engines.
	for en in all_engines if en:ignition and not en:flameout {
	  set thrust to thrust + en:availablethrust.
	  set isp to isp + en:isp.
	  set engine_count to engine_count + 1.
	}
	set isp to isp / engine_count.
	set thrust to thrust* 1000.// Engine Thrust (kg * m/s²)
	return (thrust/(g * isp)). //kg of change
}/// End Function

function f_clearLine {
	parameter line.
	local i is 0.
	local s is "".
	until i = terminal:width {
		set s to " " + s.
		set i to i + 1.
	}
	print s at (0,line).
}

function f_rel_fairing{

	FOR module IN SHIP:MODULESNAMED("ProceduralFairingDecoupler") { // Procedural Fairings
		IF module:HASEVENT("Jettison") {
			module:DOEVENT("Jettison").
		}
	}.

	FOR module IN SHIP:MODULESNAMED("ModuleProceduralFairing") { // Stock and KW Fairings
		IF module:HASEVENT("deploy") {
			module:DOEVENT("deploy").
		}
	}.
}

FUNCTION hf_mAngle{
PARAMETER a.
  UNTIL a >= 0 { SET a TO a + 360. }
  RETURN MOD(a,360).
  
}
Function ff_Avionics_off{
	Local P is SHIP:PARTSNAMED(core:part:Name)[0].
	Local M is P:GETMODULE("ModuleProceduralAvionics").
	If M:HasEVENT("Shutdown Avionics"){
		M:DOEVENT("Shutdown Avionics").
	}
}

Function ff_Avionics_on{
	Local P is SHIP:PARTSNAMED(core:part:Name)[0].
	Local M is P:GETMODULE("ModuleProceduralAvionics").
	If M:HasEVENT("Activate Avionics"){
		M:DOEVENT("Activate Avionics").
	}
}

Function hf_360AngDiff{
	Parameter a, b.
	return 180 - abs(abs(a-b)-180). 
}
Function hf_180AngDiff{
	Parameter a, b.
	return 90 - abs(abs(a-b)-90). 
}