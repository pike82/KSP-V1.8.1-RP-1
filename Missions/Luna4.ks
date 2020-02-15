// Get Mission Values
local wndw is gui(300).
set wndw:x to 700. //window start position
set wndw:y to 120.

Print "Locking resources".
Local RSS_partlist is list().
Local partlist is List().
Local resourcelist is list().
LIST Parts IN partList. 
FOR Part IN partList {  
	IF (Part:tag >= "Upperstage") or (Part:tag >= "Probe") { 
		RSS_partlist:add(Part).
	}
}
For part in RSS_partlist{
	For res in part:Resources{
			Set res:enabled to false.
	}
}

local label is wndw:ADDLABEL("Enter Mission Values").
set label:STYLE:ALIGN TO "CENTER".
set label:STYLE:HSTRETCH TO True. // Fill horizontally

local box_MoonEND_s is wndw:addhlayout().
	local MoonEND_label_s is box_MoonEND_s:addlabel("Moon PE start (s)").
	local MoonENDvalue_s is box_MoonEND_s:ADDTEXTFIELD("30").
	set MoonENDvalue_s:style:width to 100.
	set MoonENDvalue_s:style:height to 18.

local box_MoonEND is wndw:addhlayout().
	local MoonEND_label is box_MoonEND:addlabel("Moon PE km").
	local MoonENDvalue is box_MoonEND:ADDTEXTFIELD("30").
	set MoonENDvalue:style:width to 100.
	set MoonENDvalue:style:height to 18.

local box_Res is wndw:addhlayout().
	local Res_label is box_Res:addlabel("Restart Location").
	local Resvalue is box_Res:ADDTEXTFIELD("0").
	set Resvalue:style:width to 100.
	set Resvalue:style:height to 18.

local somebutton is wndw:addbutton("Confirm").
set somebutton:onclick to Continue@.

// Show the GUI.
wndw:SHOW().
LOCAL isDone IS FALSE.
UNTIL isDone {
	WAIT 1.
}

Function Continue {

	set val to MoonENDvalue_s:text.
	set val to val:tonumber(0).
	set endPE_s to val.

	set val to MoonENDvalue:text.
	set val to val:tonumber(0)*1000.
	set endPE to val.

	set val to Resvalue:text.
	set val to val:tonumber(0).
	set runmode to val.

	wndw:hide().
  	set isDone to true.
}

Global boosterCPU is "AethonB".
Global PrimTarget is moon.
//##############################################
If runmode = 0{
	Print "Waiting for activation".
	//wait for active
	Local holdload is false. 
	until holdload = true {
		Set holdload to true. //reset to true and rely on previous stage to turn false
		local PROCESSOR_List is list().
		LIST PROCESSORS IN PROCESSOR_List. // get a list of all connected cores
		for Processor in PROCESSOR_List {
			if Processor:TAG = boosterCPU{ //checks to see if previous stage is present
				Set holdload to false.
			}
		}
		wait 0.2.
	}
	Print "Luna4 active".
	Lock Throttle to 0.
	Set SHIP:CONTROL:PILOTMAINTHROTTLE TO 0.
	ff_COMMS().
	LOCK STEERING TO PROGRADE.
	RCS on.
	///unlock resources for use
	Print "Unlocking resources".
	Local RSS_partlist is list().
	Local partlist is List().
	Local resourcelist is list().
	LIST Parts IN partList. 
	FOR Part IN partList {  
		IF (Part:tag >= "Upperstage") or (Part:tag >= "Probe") { 
			RSS_partlist:add(Part).
		}
	}
	For part in RSS_partlist{
		For res in part:Resources{
				Set res:enabled to True.
		}
	}
	wait 1.
	//Circularise burn
	Lock Horizon to VXCL(UP:VECTOR, VELOCITY:SURFACE). //negative velocity makes it retrograde
	LOCK STEERING TO LOOKDIRUP(ANGLEAXIS(0,
				VCRS(horizon,BODY:POSITION))*horizon,
				FACING:TOPVECTOR).//lock to prograde along horizon
	until (ETA:apoapsis) < 150{
		wait 0.5.
	}
	SET SHIP:CONTROL:FORE TO 0.9.//start ullage using RCS
	wait 2.
	Lock Throttle to 1.
	SET SHIP:CONTROL:FORE to 0.
	Stage.//start engine
	until ship:periapsis > 140000{
		// if AVAILABLETHRUST < 0.1{
		// 	Stage.
		// }
		Wait 0.01.
	}
	Local englist is List().
	LIST ENGINES IN engList. 
	FOR eng IN engList {  
		Print "eng:STAGE:" + eng:STAGE.
		Print STAGE:NUMBER.
		IF eng:STAGE >= STAGE:NUMBER { 
				eng:shutdown. 
		}
	}
	Lock Throttle to 0.
	Stage.
	Wait 1.
	Stage.//move to next engine
	RCS off.
	Panels on.
	Set runmode to 1.
}
If runmode = 1{
	////Transfer to moon
	Local transnode is ff_Pro_transfer().
	local transmnv is node(transnode[0], transnode[1], transnode[2], transnode[3]).
	add transmnv.
	ff_Avionics_off().
	set runmode to 2.
}
If runmode = 2{
	local startTime is time:seconds + nextnode:eta - (ff_Burn_Time(nextnode:deltaV:mag, 285, 71, 1) / 2).
	Print "burn starts at: " + startTime.
	Print nextnode:orbit:nextPatch:inclination.
	wait 5.
	warpto(startTime - 150).//not always good for patched conics
	wait until time:seconds > startTime - 120.
	ff_Avionics_on().
	RCS on.
	lock steering to nextnode:burnvector.
	//SAS on.
	//Set SASMODE to "MANEUVER". //SAS tend to use less RCS than KOS steering but must have connection and good SAS core
	wait until time:seconds > (startTime-10).//RCS ullage Start
	SET SHIP:CONTROL:FORE to 1.
	//SAS off.
	//lock steering to nextnode:burnvector.
	wait until time:seconds > startTime.
	lock throttle to 1.
	// //Re-start main engines
	// Local englist is List().
	// LIST ENGINES IN engList. 
	// FOR eng IN engList {  
	// 	Print "eng:STAGE:" + eng:STAGE.
	// 	Print STAGE:NUMBER.
	// 	IF eng:STAGE >= STAGE:NUMBER { 
	// 		eng:activate. 
	// 		Print "Engine". 
	// 	}
	// }
	SET SHIP:CONTROL:FORE to 0.
	until hf_isManeuverComplete(nextnode) {
		if ship:orbit:HASNEXTPATCH {
			if ship:orbit:nextPatch:periapsis < endPE {
				Break.
			}
		}
		// if AVAILABLETHRUST < 0.1{
		// 	Stage.
		// 	Wait 1.
		// }
		wait 0.001.
	}
	lock throttle to 0.
	unlock steering.
	RCS off.
	wait 1.
	//Stage.//move to RCS engines
	remove nextnode.
	ff_avionics_off().
	wait 10.
	remove nextnode.
	set runmode to 3.
}
If runmode = 3{
	// Set corr_time to time:seconds + (ship:orbit:nextPatchEta/2).
	// Print "corr starts at: " + corr_time.
	// wait 5.
	// ff_Avionics_off().
	// warpto(corr_time - 25).//not always good for patched conics
	// until time:seconds > corr_time {
	// 	Wait 1.
	// }
	// ff_Avionics_on().
	// //Stage.//activate RCS engines
	// Local transnode is ff_Pro_Transfer(moon, 0).
	// local transmnv is node(transnode[0], transnode[1], transnode[2], transnode[3]).
	// add transmnv.
	// local startTime is time:seconds + transmnv:eta - (ff_Burn_Time(transmnv:deltaV:mag, 285, 71, 1) / 2).
	// Print "burn starts at: " + startTime.
	// wait 5.
	// ff_Avionics_off().
	// warpto(startTime - 90).//not always good for patched conics
	// RCS on.
	// lock steering to nextnode:burnvector.
	// if nextnode:burnvector:mag > 15{
	// 	ff_Avionics_on().
	// 	wait until time:seconds > (startTime-10).//RCS ullage Start
	// 	SET SHIP:CONTROL:FORE to 1.
	// 	wait 10.
	// 	SET SHIP:CONTROL:FORE to 0.
	// 	lock throttle to 1.
	// 		//Re-start main engines
	// 	Local englist is List().
	// 	LIST ENGINES IN engList. 
	// 	FOR eng IN engList {  
	// 		Print "eng:STAGE:" + eng:STAGE.
	// 		Print STAGE:NUMBER.
	// 		IF eng:STAGE >= STAGE:NUMBER { 
	// 			eng:activate. 
	// 			Print "Engine". 
	// 		}
	// 	}
	// 	until hf_isManeuverComplete(nextnode) {
	// 		if ship:orbit:HASNEXTPATCH {
	// 			if ship:orbit:nextPatch:periapsis < endPE{
	// 				Break.
	// 			}
	// 		}
	// 		wait 0.001.
	// 	}
	// 	lock throttle to 0.
	// }
	// remove nextnode.
	// ff_Avionics_off().
	// RCS off.
	// remove nextnode.
	set runmode to 4.
}
If runmode = 4{
	// Set corr_time to time:seconds + (ship:orbit:nextPatchEta).
	// until time:seconds +60 > corr_time {
	// 	Wait 1.
	// }
	// until orbit:body = moon{
	// 	wait 1.0.
	// }
	// Print "In SOI: " + time:clock.
	// wait 180.
	// local normalVec is vcrs(ship:velocity:orbit,-body:position).
	// local radialVec is vcrs(ship:velocity:orbit,normalVec).
	// Print "Waiting for PE burn".

	// // Lock sinc to ship:orbit:inclination.
	// ff_avionics_on().
	// If ship:orbit:periapsis < endPE{
	// 	Print "PE Change".
	// 	RCS on.
	// 	lock Steering to -radialVec.
	// 	wait 60.
	// 	SET SHIP:CONTROL:FORE to 1.
	// 	Until ship:orbit:periapsis > endPE{
	// 		Wait 0.01.
	// 	}
	// 	SET SHIP:CONTROL:FORE to 0.
	// 	RCS off.
	// }
	// wait 5.0.
	// If ship:orbit:periapsis > endPE{
	// 	Print "PE Change 2".
	// 	RCS on.
	// 	lock Steering to radialVec.
	// 	wait 60.
	// 	SET SHIP:CONTROL:FORE to 1.
	// 	Until ship:orbit:periapsis > endPE{
	// 		Wait 0.01.
	// 	}
	// 	SET SHIP:CONTROL:FORE to 0.
	// 	RCS off.
	// }
	// RCS off.
	// ff_avionics_off().
	set runmode to 5.
}
If runmode = 5{
	until orbit:body = moon{
		wait 1.0.
	}
	Print "Waiting for PE burn".
	Print "PE Burn Setup".
	Local orbspeed is sqrt(Body:MU/(endPE + body:radius)).
	Print velocityat(ship, eta:periapsis):orbit:mag.
	Print velocityat(Ship:Body, eta:periapsis):orbit:mag.
	Local BurnSpeed is velocityat(ship, eta:periapsis):orbit:mag - orbspeed.
	If ship:periapsis < 0 {
		set endPE_s to 180.// if pe underground need to commence burn atleast 3 minutes away from PE. If it can't do this it will not have enough fuel.
	}
	Set corr_time to (time:seconds + eta:periapsis - endPE_s).
	Print eta:periapsis.
	Print "Dv: " +BurnSpeed.
	Print corr_time. 
	wait 5.
	warpto (corr_time-180).
	wait 15.
	ff_avionics_on().
	Lock steering to retrograde.
	RCS on.
	Set warp to 0.
	until eta:periapsis < endPE_s{
		wait 2.
	}
	SET SHIP:CONTROL:FORE to 1.
	wait 5.
	SET SHIP:CONTROL:FORE to 0.
	lock throttle to 1.
	Print "Starting PE Burn".
	//Re-start main engines
	Local englist is List().
	LIST ENGINES IN engList. 
	FOR eng IN engList {  
		Print "eng:STAGE:" + eng:STAGE.
		Print STAGE:NUMBER.
		IF eng:STAGE >= STAGE:NUMBER { 
			eng:activate. 
			Print "Engine". 
		}
	}
	wait 15.
	until (AVAILABLETHRUST*1000) < 1{
		wait 0.1.
	}
	lock throttle to 0.
	Stage.//move to lander only
	wait 1.
	Stage. //activate lander engine
	//Commence lander script
	Set runmode to 6.
}
If runmode = 6{
	//commence Landing routine
	Lock steering to retrograde.
	ff_CAB().
	ff_SuBurn(198, 1, 0.1, 500).
	wait 2.
	ff_SuBurn(198, 1).
	wait 400.
}
Shutdown.
//##############################################
//Transfer Functions
function ff_Pro_Transfer { //ideal for prograde only transferes
  	Parameter target is PrimTarget, First_Est is ff_Hohmann(moon). //
	Local start is time:seconds + 60.
	Local end is time:seconds + 10000.
	If (orbit:apoapsis <0) or (orbit:apoapsis > 40000000){ // orbit period = inf or space high
		Set end to time:seconds + 120 + 400.
	}	else{
				If orbit:period < 10000{
					Set end to time:seconds + 60 + orbit:period.
				}
				if orbit:period > 10000{
					Set end to time:seconds + 60 + 40000.
				}
	}
  	local startSearchTime is hf_ternarySearchparam(hf_angleToMoon@, target, start, end, 1).
  	local transfer is ff_seek (startSearchTime, ff_freeze(0),ff_freeze(0),first_est, hf_moonTransferScore@).// t, RADIALOUT, NORMAL, PROGRADE, func 
	Set transfer to list( transfer[0], hf_unfreeze(transfer[1]), hf_unfreeze(transfer[2]), transfer[3] ).
  	return transfer.
}

function ff_Transfer {
  	Parameter target is PrimTarget, First_Est is ff_Hohmann(moon). //
	Local start is time:seconds + 60.
	Local end is time:seconds + 10000.
	If (orbit:apoapsis <0) or (orbit:apoapsis > 40000000){ // orbit period = inf or space high
		Set end to time:seconds + 120 + 400.
	}	else{
				If orbit:period < 10000{
					Set end to time:seconds + 60 + orbit:period.
				}
				if orbit:period > 10000{
					Set end to time:seconds + 60 + 40000.
				}
	}
  	local startSearchTime is hf_ternarySearchparam(hf_angleToMoon@, target, start, end, 1).
  	local transfer is ff_seek (startSearchTime, 0, 0, first_est, hf_moonTransferScore@).// t, RADIALOUT, NORMAL, PROGRADE, func 
  	return transfer.
}

function hf_ternarySearch {
  parameter f, left, right, absolutePrecision.
  until false {
    if abs(right - left) < absolutePrecision {
      return (left + right) / 2.
    }
    local leftThird is left + (right - left) / 3.
    local rightThird is right - (right - left) / 3.
    if f(leftThird) < f(rightThird) {
      set left to leftThird.
    } else {
      set right to rightThird.
    }
  }
}

function hf_ternarySearchparam {
  parameter f, f_para, left, right, absolutePrecision.
  until false {
    if abs(right - left) < absolutePrecision {
      return (left + right) / 2.
    }
    local leftThird is left + (right - left) / 3.
    local rightThird is right - (right - left) / 3.
    if f(leftThird,f_para) < f(rightThird,f_para) {
      set left to leftThird.
    } else {
      set right to rightThird.
    }
  }
}

function hf_angleToMoon {
  	parameter t, Target is PrimTarget.
  	return vectorAngle(
    Ship:body:position - positionAt(ship, t),
    Ship:body:position - positionAt(Target, t)
  	).
}

function hf_moonTransferScore {
  	parameter mnv, targ is PrimTarget.
  	local result is 0.
  	if mnv:orbit:hasNextPatch {
	  	if mnv:burnvector:mag < 3200{//specific for earth to moon transfer
    		set result to -abs((mnv:orbit:nextPatch:periapsis - endPE)) -((mnv:burnvector:mag)*100)- ((mnv:orbit:nextPatch:inclination)*10).
	 	} Else{
			Set result to -10*abs((mnv:orbit:nextPatch:periapsis - endPE)) -((mnv:burnvector:mag)*100)- ((mnv:orbit:nextPatch:inclination)*10).
		}
 	 } else {
    set result to -10000000*hf_distanceToMoonAtApoapsis(mnv, targ).
  	}
  	print "results: " + result.
  	Print targ.
  	return result.
}

function hf_distanceToMoonAtApoapsis {
  parameter mnv, targ is PrimTarget.
  local apoapsisTime is hf_ternarySearch(
    hf_altitudeAt@,
    time:seconds + mnv:eta, 
    time:seconds + mnv:eta + (mnv:orbit:period / 2),
    1
  ).
  return (positionAt(ship, apoapsisTime) - positionAt(targ, apoapsisTime)):mag.
}

function hf_altitudeAt {
  parameter t.
  return ship:body:altitudeOf(positionAt(ship, t)).
}

function ff_seek {
	parameter t, r, n, p, fitness, fine is False,
			  data is list(t, r, n, p),
			  fit is hf_orbit_fitness(fitness@).  // time, radial, normal, prograde, fitness are the parameters passed in for the node to be found. passes fitness through as a delegate to orbital fitness in this case { parameter mnv. return -mnv:orbit:eccentricity. } is passed through as a local function but any scorring evaluation can be passed through
	set data to ff_optimize(data, fit, 30).
	Print "Seek 30".
	wait 2.
	set data to ff_optimize(data, fit, 10).
	Print "Seek 10".
	wait 2.
	set data to ff_optimize(data, fit, 1). 
	Print "Seek 1".
	wait 2.
	If Fine{
		set data to ff_optimize(data, fit, 0.1). // search in 0.1m/s incriments
		Print "Seek 0.1".
	}
	fit(data). //sets the final manuver node and returns its parameters
	wait 0. 
	return data. // returns the manevour node parameters to where the function was called
}/// End Function

function ff_optimize {
	parameter data, fitness, step_size,
	winning is list(fitness(data), data),
	improvement is hf_best_neighbor(winning, fitness, step_size). 
	until improvement[0] <= winning[0] { 
	  	set winning to improvement. 
	  	set improvement to hf_best_neighbor(winning, fitness, step_size). 
	}
	return winning[1]. // returns the second column of the winning list "(data)", instead of "fitness(data)"
 }/// End Function

function ff_freeze {
	parameter n. 
	return lex("frozen", n).
}/// End Function


// identifies if the paramter is frozen
function hf_frozen {
	parameter v. 
	return (v+""):indexof("frozen") <> -1.
}/// End Function

// Returns paramters from the frozen lexicon
function hf_unfreeze {
	parameter v. 
	if hf_frozen(v) return v["frozen"]. 
	else return v.
}/// End Function
	
function hf_orbit_fitness {
	parameter fitness. // the parameter used to evaluate fitness
	return {
		parameter data.
		until not hasnode { 
			remove nextnode. // Used to remove any existing nodes
			wait 0. 
		} 
		Print "orb fit create node".
		local new_node is node(
		hf_unfreeze(data[0]), hf_unfreeze(data[1]),
		hf_unfreeze(data[2]), hf_unfreeze(data[3])). 
		add new_node. 
		//Print new_node.
		wait 0.
		return fitness(new_node). // returns the manevour node parameters to where the function was called
	}.
}/// End Function
	
function hf_best_neighbor {
	parameter best, fitness, step_size. 
	for neighbor in hf_neighbors(best[1], step_size) { 
		local score is fitness(neighbor). 
		if score > best[0] set best to list(score, neighbor). 
	}
	return best. 
}/// End Function

function hf_neighbors {
	parameter data, step_size, results is list().
	for i in range(0, data:length) if not hf_frozen(data[i]) { 
		local increment is data:copy.
		local decrement is data:copy.
		set increment[i] to increment[i] + step_size. 
		set decrement[i] to decrement[i] - step_size. 
		results:add(increment).
		results:add(decrement).
	}
	return results. // Return the list of neighbours for the data that can be changed (i.e. unfrozen)
}  /// End Function	

Function ff_Hohmann{

PARAMETER tgt, t_pe is 0, trans_bod is Ship:BODY, inc_tgt is 0. // trans_bod should be sun for planet transfers
	
	Local Curr_time is time:seconds.
	LOCAL Ship_Orbit is ORBITAT(SHIP,Curr_time). //ORBITAT(orbitable,time) is KOS in-built function
	LOCAL tgt_Orbit is ORBITAT(tgt,Curr_time).
	LOCAL r1 is Ship_Orbit:SEMIMAJORAXIS.
	LOCAL r2 is tgt_Orbit:SEMIMAJORAXIS + t_pe.

	LOCAL dvDepart is SQRT(trans_bod:MU/r1) * (SQRT((2*r2)/(r1+r2)) -1). // wiki Dv1 Equation
	LOCAL dvArrive is SQRT(trans_bod:MU/r1) * (1- SQRT((2*r2)/(r1+r2))). // wiki Dv2 Equation
	
	if r2 < r1 { 
		SET dvDepart TO -dvDepart. // this allows for transfers to a lower orbit
		SET dvArrive TO -dvArrive.
	}
	
	if -dvDepart = dvArrive {
		set dvArrive to 0. // allows for transfers within the same SOI where the dv arrive and depart are the same.
	}

	local dv is dvDepart + dvArrive.
	LOCAL Trans_time is CONSTANT:PI * SQRT( ((r1+r2)^3) / (8 * trans_bod:MU) ). // wiki transfer orbit time Equation
	LOCAL Tgt_travel_ang is (Trans_time / tgt_Orbit:PERIOD)* 360. // the angle the tgt moves during the transist assuming a circular orbit
	LOCAL desired_phi is 180 - Tgt_travel_ang. // we want to meet the target at apoapsis so the target need to travel and end 180 degrees from where we start.
	LOCAL rel_ang_Change is (360 / Ship_Orbit:PERIOD) - (360 / tgt_Orbit:PERIOD). // the degrees the tgt moves each orbit by the ship each second.
	LOCAL ship_pos is positionat(SHIP, Curr_time)-ship:body:position. //current position of the ship
	LOCAL tgt_pos is positionat(tgt, Curr_time)-tgt:body:position. //current position of the target
	LOCAL start_phi is VANG(ship_pos,tgt_pos). // the current angle between the ship and the tgt.
	LOCAL ship_normal IS VCRS(VELOCITYAT(SHIP,curr_time):ORBIT,ship_pos).// the plane of the ship
	LOCAL ship_tgt_cross IS VCRS(ship_pos,tgt_pos).//// plane of the transfer (ie. incination diference)
	
	if VDOT(ship_normal, ship_tgt_cross) > 0 { 
		SET start_phi TO 360 - start_phi. 
	} // this checks to see if the planes are pointed in the same direction or are pointed opposite to one another so it is known if ship is leading or lagging the tgt. 

	LOCAL phi_delta is hf_mAngle(start_phi - desired_phi). //this determines how far off the best phase angle is.
	if rel_ang_Change < 0 { 
		SET phi_delta TO phi_delta - 360. //adjust for negative angle change values
	}
	Local node_time is Curr_time + (phi_delta / rel_ang_Change).
  return dv.
}
////##############################################
//Landing Functions
Function ff_CAB{ 
	//this landing tries to burn purely horizontal and uses a pid to determine the desired downwards velocity and cancel it out through a pitch change. It does not stop the throttle or point upwards, that is upto the user to code in or allow a transistion into another function.

	Parameter ThrottelStartTime is 0.1, SafeAlt is 10, TargetLatLng is "Null", EndHorzVel is 0. // throttle start time is the time it take the trottle to get up to full power TODO: have this also take into account the rotation of the body so it can target a specific landing spot.
	
	Set PEVec to velocityat(Ship, ETA:PERIAPSIS + TIME:SECONDS):Surface.
	Set Horzvel to PEVec:mag. // its known at PE the verVel is Zero so all velocity must in theory be horizontal	

	local BurnStartTime is time:seconds + ETA:PERIAPSIS.
	If orbit:periapsis < 15000{
		Set BurnStartTime to time:seconds.
	}
	
	Until time:seconds > (BurnStartTime-ThrottelStartTime){
		clearscreen.
		Print "Burn Horizontal Velocity to Cancel:" + PEVec:mag.
		Print "Wating for CAB Start in :" + (BurnStartTime - Time:seconds-ThrottelStartTime).
		Lock steering to ship:retrograde. 
		wait 0.001.
	}
	
	Set PIDVV to PIDLOOP(0.03, 0, 0.1, -0.1, 0.1).//SET PID TO PIDLOOP(KP, KI, KD, MINOUTPUT, MAXOUTPUT).	
	Set PIDVV:SETPOINT to 0. // we want the altitude to remain constant so no vertical velocity.
	Set highpitch to 0.

	//lock steering to retrograde * r(-highPitch, 0, 0):vector.
	Lock Horizon to VXCL(UP:VECTOR, -VELOCITY:SURFACE). //negative makes it retrograde
	LOCK STEERING TO LOOKDIRUP(ANGLEAXIS(-highPitch,
                        VCRS(horizon,BODY:POSITION))*horizon,
						FACING:TOPVECTOR).//lock to retrograde at horizon
	
	Set Basetime to time:seconds.
	Lock Throttle to 1.0.
	Until ETA:PERIAPSIS > 500{
		//Create PID to adjust the craft pitch (without thrusting downward) which maintains a vertical velocity of zero and regulates the velocity of burn height change if not zero reventing a pitch above the horizontal.		
		Clearscreen.
		Print "Undertaking inital CAB".
		Print "Ground Speed: " + SHIP:GROUNDSPEED.
		Print "Pitch: " + highpitch.
		wait 0.01.
	}
	Until SHIP:GROUNDSPEED < 800{
		//Create PID to adjust the craft pitch (without thrusting downward) which maintains a vertical velocity of zero and regulates the velocity of burn height change if not zero reventing a pitch above the horizontal.		
		Set dpitch TO PIDVV:UPDATE(TIME:SECONDS, verticalspeed). //Get the PID on the AlT diff as desired vertical velocity
		Set highpitch to min(max(highpitch + dpitch,0),60). // Ensure the pitch does not go below zero as gravity will efficently lower the veritcal velocity if required. also prevent it going above 90.
		Clearscreen.
		Print "Undertaking CAB".
		Print "Ground Speed: " + SHIP:GROUNDSPEED.
		Print "Pitch: " + highpitch.
		wait 0.01.
	}
	Set PIDVV:SETPOINT to -50. // we want the altitude to start reducing.
	Until SHIP:GROUNDSPEED < 50 {
		//Create PID to adjust the craft pitch (without thrusting downward) which maintains a vertical velocity of zero and regulates the velocity of burn height change if not zero reventing a pitch above the horizontal.		
		Set dpitch TO PIDVV:UPDATE(TIME:SECONDS, verticalspeed). //Get the PID on the AlT diff as desired vertical velocity
		Set highpitch to min(max(highpitch + dpitch,0),60). // Ensure the pitch does not go below zero as gravity will efficently lower the veritcal velocity if required
		Clearscreen.
		Print "Undertaking retrograde transistion".
		Print "Ground Speed: " + SHIP:GROUNDSPEED.
		Print "Pitch: " + highpitch.
		wait 0.01.
	}
	Lock steering to retrograde.
	Print "Canceling ground speed".
	Until SHIP:GROUNDSPEED < 10 {
		wait 0.01.
	}
	Lock Throttle to 0.0.
} //End of Function

Function ff_SuBurn {

	Parameter EngISP, EngThrust, ThrottelStartUp is 0.1, SafeAlt is 5, EndVelocity is (-1.5). // end velocity must be negative
	Lock Throttle to 0.0.
	local Flight_Arr is lexicon().
	set Flight_Arr to hf_fall(EngThrust).
	Lock steering to retrograde.
	Until (Flight_Arr["fallDist"] + SafeAlt + (ThrottelStartUp * abs(ship:verticalspeed))) > (ship:Altitude - SHIP:GEOPOSITION:TERRAINHEIGHT) { // until the radar height is at the suicide burn height plus safe altitude and an allowance for the engine to throttle up to max thrust
		//Run screen update loop to inform of suicide burn wait.
		Set Flight_Arr to hf_fall(EngThrust).
		Clearscreen.
		Print "gl_fallTime:" + Flight_Arr["fallTime"].
		Print "gl_fallVel:" + Flight_Arr["fallVel"].
		Print "gl_fallDist:" + Flight_Arr["fallDist"].
		Print "gl_fallBurnTime:" + ff_burn_time(Flight_Arr["fallVel"],EngISP, EngThrust).
		Print "Radar Alt:" + (ship:Altitude - SHIP:GEOPOSITION:TERRAINHEIGHT).
		Wait 0.001.
	}
	//Burn Height has been reached start the burn
	until 0 {
		If verticalspeed > EndVelocity{
		 Lock Throttle to 0.0.
		} else {
			Lock Throttle to 1.0.
		}.
		
		if abs(verticalspeed) < 20 {
			LOCK STEERING to HEADING(90,90). // Lock in upright posistion and fixed rotation
		}.
		if (((ship:Altitude - SHIP:GEOPOSITION:TERRAINHEIGHT) < 0.25) or (Ship:Status = "LANDED")) or (SafeAlt > 5) { // this is used if the burn is intended to land the craft.
			Lock Throttle to 0.
			Unlock Throttle.
			Break.
		}.
		Wait 0.01.
	} // end Until
	// Note: if the ship does not meet these conditions the throttle will still be locked at 1, you will need to ensure a landing has taken place or add in another section in the runtime to ensure the throttle does not stay at 1 an make the craft go back upwards.
} //End of Function

////#################################################
//General Functions

function hf_isManeuverComplete {
	parameter mnv.
	if not(defined originalVector) or originalVector = -1 {
		declare global originalVector to mnv:burnvector.
	}
	if vang(originalVector, mnv:burnvector) > 45 {
		declare global originalVector to -1.
		return true.
	}
	return false.
}

Function ff_stage_delta_v {
Parameter RSS_partlist is list().
//Calculates the amount of delta v for the current stage    
local m is ship:mass * 1000. // Starting mass (kg)
local g is 9.80665.
local engine_count is 0.
local isp is 0. // Engine ISP (s)
local RSS is False.
local fuelmass is 0.
	// obtain ISP
	LIST engines IN engList.
	for en in engList 
	if en:ignition and not en:flameout {
	  set isp to isp + en:isp.
	  set engine_count to engine_count + 1.
	}
	set isp to isp / engine_count.
	
	// obtain RSS yes or no.
	for res IN Stage:Resources{
		if res:name = "HTP"{
			Set RSS to true.
		}
	}
	Print "RSS" + RSS.
	If RSS = true{
	//for real fuels 
		local fuels is list("LQDOXYGEN", "LQDHYDROGEN", "KEROSENE", "Aerozine50", "UDMH", "NTO", "MMH", 
			"HTP", "IRFNA-III", "NitrousOxide", "Aniline", "Ethanol75", "LQDAMMONIA", "LQDMETHANE", 
			"CLF3", "CLF5", "DIBORANE", "PENTABORANE", "ETHANE", "ETHYLENE", "OF2", "LQDFLUORINE", 
			"N2F4", "FurFuryl", "UH25", "TONKA250", "TONKA500", "FLOX30", "FLOX70", "", "FLOX88", 
			"IWFNA", "IRFNA-IV", "AK20", "AK27", "CaveaB", "MON1", "MON3", "MON10", "MON15", "MON20", "Hydyne", "TEATEB").
		for tankPart in RSS_partlist{
			for res in tankpart:RESOURCES{
				for f in fuels{
					if f = res:NAME{
						SET fuelMass TO fuelMass + ((res:DENSITY*res:AMOUNT)*1000).
						Print "fuel mass" + fuelmass.
					}
				}
			}
		}
	} Else {
	//for stock fuels
		local fuels is list("LiquidFuel", "Oxidizer", "SolidFuel", "MonoPropellant").
		for res in STAGE:RESOURCES{
			for f in fuels{
				if f = res:NAME{
					SET fuelMass TO fuelMass + res:DENSITY*res:AMOUNT.
				}
			}
		}
	}
	//TODO:Think about removing RCS components or making it an input term as this could be a significant proportion of the deltaV which is not used.
	return (isp * g * ln(m / (m - fuelMass))).
}./// End Function

function ff_burn_time {
parameter dV, isp is 0, thrust is 0, engine_count is 0. // For RSS/RO engine values must be given unless they are actually burning.
lock throttle to 0.
Print "Burntime".
	local g is 9.80665.  // Gravitational acceleration constant used in game for Isp Calculation (m/s²)
	local m is ship:mass * 1000. // Starting mass (kg)
	local e is constant():e. // Base of natural log
	
	//TODO: look at comapring the dv with the ff_stage_delta_v. If less look at the engine in the next stage and determine the delta_v and time to burn until the dv has been meet.
	If engine_count = 0{ // only evaluate is figures not given
		list engines in all_engines.
		for en in all_engines {
			if en:ignition and not en:flameout {
				set thrust to thrust + en:availablethrust.
				set isp to isp + en:isp.
				set engine_count to engine_count + 1.
			}
		}
	}
	if engine_count = 0{
		return 1. //return something to prevent error.
	}
	set isp to isp / engine_count. //assumes only one type of engine in cluster
	set thrust to thrust * 1000. // Engine Thrust (kg * m/s²)
	Print isp.
	Print Thrust.
	return g * m * isp * (1 - e^(-dV/(g*isp))) / thrust.
}/// End Function

FUNCTION hf_mAngle{
PARAMETER a.
  UNTIL a >= 0 { SET a TO a + 360. }
  RETURN MOD(a,360).
  
}
FUNCTION ff_COMMS {
	PARAMETER event is "activate", stagewait IS 0.1, ShipQtgt is 0.0045.
	// "deactivate"
	IF SHIP:Q < ShipQtgt {
		FOR antenna IN SHIP:MODULESNAMED("ModuleRTAntenna") {
			IF antenna:HASEVENT(event) {
				antenna:DOEVENT(event).
				PRINT event + " Antennas".
				WAIT stageWait.
			}	
		}.
	}
} // End of Function

function ff_mdot {
	Parameter RSS_true is 0.
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
	If engine_count = 0{
		Set engine_count to 1.
	}
	set isp to isp / engine_count.
	set thrust to thrust* 1000.// Engine Thrust (kg * m/s²)
	If RSS_true = 0{
		return (thrust/(g * isp)). //kg of change	
	}
	return RSS_true.
}/// End Function

function ff_Vel_Exhaust {
	Parameter RSS_true is 0.
	local g is 9.80665.  
	local engine_count is 0.
	local thrust is 0.
	local isp is 0. // Engine ISP (s)
	list engines in all_engines.
	for en in all_engines if en:ignition and not en:flameout {
	  set thrust to thrust + en:availablethrust.
	  set isp to isp + en:isp.
	  set engine_count to engine_count + 1.
	}
	If engine_count = 0{
		Set engine_count to 1.
	}
	set isp to isp / engine_count.
	If RSS_true = 0{
		return g *isp.///thrust). //
	}
	return RSS_true*g.
}/// End Function

Function hf_Fall{
	Parameter s_thrust is ship:AVAILABLETHRUST.
//Fall Predictions and Variables
	local gl_Grav is ff_Gravity().
	local fallTime is ff_quadraticPlus(-gl_Grav["Avg"]/2, -ship:verticalspeed, ship:Altitude - SHIP:GEOPOSITION:TERRAINHEIGHT).//r = r0 + vt - 1/2at^2 ===> Quadratic equiation 1/2*at^2 + bt + c = 0 a= acceleration, b=velocity, c= distance
	local fallVel is abs(ship:verticalspeed) + (gl_Grav["Avg"]*fallTime).//v = u + at
	local fallAcc is (s_thrust/ship:mass). // note is is assumed this will be undertaken in a vaccum so the thrust and ISP will not change. Otherwise if undertaken in the atmosphere drag will require a variable thrust engine so small variations in ISP and thrust won't matter becasue the thrust can be adjusted to suit.
	local fallDist is (fallVel^2)/ (2*(fallAcc)). // v^2 = u^2 + 2as ==> s = ((v^2) - (u^2))/2a 

	local arr is lexicon().
	arr:add ("fallTime", fallTime).
	arr:add ("fallVel", fallVel).
	arr:add ("fallAcc", fallAcc).
	arr:add ("fallDist", fallDist).
	
	Return(arr).
}

function ff_Gravity{
	Parameter Surface_Elevation is SHIP:GEOPOSITION:TERRAINHEIGHT.
	Set SEALEVELGRAVITY to body:mu / (body:radius)^2. // returns the sealevel gravity for any body that is being orbited.
	Set GRAVITY to body:mu / (ship:Altitude + body:radius)^2. //returns the current gravity experienced by the vessel	
	Set AvgGravity to sqrt(		(	(GRAVITY^2) +((body:mu / (Surface_Elevation + body:radius)^2 )^2)		)/2		).// using Root mean square function to find the average gravity between the current point and the surface which have a squares relationship.
	local arr is lexicon().
	arr:add ("SLG", SEALEVELGRAVITY).
	arr:add ("G", GRAVITY).
	arr:add ("AVG", AvgGravity).
	Return (arr).
}

function ff_quadraticPlus {
	parameter a, b, c.
	return (b - sqrt(max(b ^ 2 - 4 * a * c, 0))) / (2 * a).
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