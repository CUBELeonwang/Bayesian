
%% Co-simulation of Energyplus and CityFFD

% Instantiate co-simulation tool
ep = mlep;

% Building simulation configuration file
ep.idfFile = 'EnergyPlus_DH';
 
% Weather file
ep.epwFile = 'QAT_DOHA-INTL-AP_411700_IW2';

%timestep = ep.timestep; %[s]
timestep = 3600;

% Simulation length
endTime = 1*24*60*60; %[s]

% Start co-simulatin
ep.start;

% The simulation loop
t = 0;
while t < endTime
%----------------EnergyPlus to CityFFD---------------------
% surface name: wind expose, sun expose
    surfName = csvread("surface_number.csv", 0, 0, [0, 0, 327, 0]);
    % Obtain elapsed simulation time
    t = ep.time; %[s]
    
   
% Get outputs from EnergyPlus
    [y, t] = ep.read;  
  
    %surface value: Surface Outside Face Temperature
    surfTemp = y(1:328).'; % transpose y
    % EnergyPlus output data
    epArr = [surfName, surfTemp];
    
    % output epArr to txt
    % open a file for writing
    fileID = fopen('epFile.txt', 'w');
    fprintf(fileID,'%6s %12s\n','surface_name','T_s');
    fprintf(fileID,'%6.0f %12.8f\n', epArr.');
    fclose(fileID);
    %type epFile.txt
    ite = t/timestep;
    copyfile("epFile.txt", "epFile_"+string(ite)+".txt");
    
    %output results of zone ideal loads zone total cooling energy
    cload = y(329:358).';
    % save cload to txt file
    dlmwrite('coolingEnergy.txt',cload);
    copyfile("coolingEnergy.txt", "coolingEnergy_"+string(ite)+".txt");

    
    % update cityffd's weather file according to epw file
    copyfile("weatherdata_"+string(ite)+".txt", "weatherdata.txt")
    %run CItyFFD
    system('CityFFD_EP.exe'); % lunch exe file in the same directory
    copyfile("cityFFDFile.txt", "cityFFDFile_"+string(ite)+".txt");
    
    %----------------CityFFD to EnergyPlus---------------------
    M = dlmread('cityFFDFile.txt', ' ', 1, 0);
    M = sortrows(M, 1); % sort the matrix according to first column; first column is surface name
    u = M (:, 2).';
    
    ep.write(u,t);    
    %fprintf('haha \n')
    %t = t + timestep;
    %type cityFFDFile.txt
    fprintf('iteration '+string(t/timestep)+' is done \n')
end

ep.stop;






