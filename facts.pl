% Allow runtime modification
:- dynamic effectorValue/2.
:- dynamic sensor/2.
:- dynamic sensorValue/2.
:- dynamic preference/4.
:- dynamic inside/1.
:- dynamic outside/1.

% -----------------------
% Sensor Definitions
% -----------------------

% Format: sensor(SensorId, Type).
sensor(light_outside, light).
sensor(light_inside, light).
sensor(temp_outside, temp).
sensor(temp_inside, temp).
sensor(wind_sensor, wind).
sensor(rain_sensor, rain).
sensor(noise_sensor, noise).

% -----------------------
% Sensor Values
% -----------------------
% Format: sensorValue(SensorId, CurrentValue).
sensorValue(light_outside, 30).
sensorValue(light_inside, 70).
sensorValue(temp_outside, 15).
sensorValue(temp_inside, 20).
sensorValue(wind_sensor, 20).
sensorValue(rain_sensor, 0).
sensorValue(noise_sensor, 80).

% -----------------------
% Sensor Location
% -----------------------

inside(light_inside).
inside(temp_inside).

outside(light_outside).
outside(temp_outside).
outside(wind_sensor).
outside(rain_sensor).
outside(noise_sensor).

% -----------------------
% Effector Values
% -----------------------
% Format: effectorValue(EffectId, CurrentState). 
% (0 = off, 1 = on or intensity level)

effectorValue(light_inside_eff, 0).
effectorValue(light_outside_eff, 0).
effectorValue(ac, 0).
effectorValue(r, 0).  % Radiator
effectorValue(noise_barrier, 0).

% -----------------------
% User Preferences
% -----------------------
% Format: preference(ProfileId, SensorType, PreferredValue, [Effectors])

preference(comfort, light, 50, [light_inside_eff, light_outside_eff]).
preference(comfort, temp, 24, [ac, r]).
preference(comfort, wind, 30, [ac]).
preference(comfort, noise, 60, [noise_barrier]).
