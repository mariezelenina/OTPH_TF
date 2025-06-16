% App beta version - Automatic Preprocessing Pipeline for EEG data
% This is the resting state alternative of App
% No guarantee that this works
% The current version is as of 19/10/2017
% By Janir Ramos da Cruz @ IST

% Split into sections and adapted for DPrata lab use by Marie Zelenina

%% START THE FUN
% Directory management
clear all
clc
CurrDir = pwd;                                  
eeglab              

% change precision to double (it should be saved, but do anyway;
%                       if it's double, the options will not be modified)
pop_editoptions( 'option_single', 0);


%--- If more than one group ---------------
% Example:
% SZPatientsDir = uigetdir([],'Path to the data of SZ Patients');
% ControlsDir = uigetd ir([],'Path to the data of Controls');
% % Groups: Patients, Controls, and Relatives
% Group = {SZPatientsDir; ControlsDir};
%------------------------------------------
SubjectsDir = 'C:\Users\user\Documents\Marie\Analysis_2019\1_Preprocessing';
Group = {SubjectsDir};

% Info about your data
epoch = 4;                              % each epoch has 4 seconds for eeg analysis
n_ch = 63;                              % number of EEG channels

% Info about the analysis
doICA = 1;                              % if wants to do ICA decomposition, set doICA = 1
d_Fs = 250;                             
LowCutOffFreq = 1; 
HighCutOffFreq = 30;                    % changed it from 100/50Hz
filterOrder = [];                       % filterOrder - leave blank and determined by eeglab

% for iGroup = 1:size(Group,1)
iGroup = 1;
SaveDir = strcat(fullfile(Group{iGroup},'Results_EC'));
mkdir(SaveDir);
    
% Subjects vhdr files
Subject_VHDR = dir(fullfile(Group{iGroup},'*.vhdr'));

% The pool of subjects
Subject_pool = {Subject_VHDR(:).name}';
    
%for iSubject = 1:size(Subject_pool,1)
iSubject = 1;    
bdfFile = fullfile(Group{iGroup},Subject_pool{iSubject}); % that equals to filename
        
% Initiate subject rejected data
bad_channels = 0;
bad_trials = 0;
trial_bad_ch = 0;
        
% Read in the BDF file using eeglab
% n eeg channels and 4 EOG, from 30 seconds to 4,5 minutes
% total of 4 minutes
%dataset = pop_biosig(bdfFile, 'channels'r, 1: n_ch+4, 'blockrange', [30 31+240]);
[dataset, com] = pop_loadbv(Group{iGroup}, Subject_pool{iSubject});
disp("Done");




% FOR EYES CLOSED ONLY:
% data is loaded in 3D: electrodes*points*periods(timepoints)
% run the following code to make it 2D
% -- Marie 2019-07-26

dataset.data = reshape(dataset.data, size(dataset.data,1), size(dataset.data,2)*size(dataset.data,3));
dataset.pnts = size(dataset.data,2);
dataset.trials = 1;
dataset.epoch = [];
dataset = eeg_checkset(dataset);
eeglab redraw;

pop_editoptions( 'option_single', 0);

disp("Done");




% Bandpass filter with 0 phase lag
dataset = pop_eegfiltnew(dataset, 'locutoff', LowCutOffFreq, 'hicutoff', HighCutOffFreq, 'filtorder', filterOrder, 'revfilt', 0, 'usefft', [], 'plotfreqz', 0);
        
% CleanLine to remove the 50 Hz and the harmonics https://www.nitrc.org/docman/view.php/572/1117/Readme
% COMMENT OUT FOR ML EXPERIMENTS
dataset = pop_cleanline(dataset, 'Bandwidth',2,'ChanCompIndices',[1:dataset.nbchan],...
                'SignalType','Channels','ComputeSpectralPower',true,               ...
                'LineFrequencies',[50 100 150 200 250] ,'NormalizeSpectrum',false, ...
                'LineAlpha',0.01,'PaddingFactor',2,'PlotFigures',false,            ...
                'ScanForLines',true,'SmoothingFactor',100,'VerboseOutput',1,       ...
                'SlidingWinLength',4,'SlidingWinStep',4);
        
% Downsample the data if needed
if dataset.srate ~= d_Fs
     dataset = pop_resample(dataset,d_Fs);
end
        
% Separate the EOG, EMG and EEG channels
chanlabels = {dataset.chanlocs.labels}';   

IndEMGtmp = strfind(chanlabels, 'emg');
IndEMG = find(not(cellfun('isempty', IndEMGtmp)));
dataset_emg = pop_select(dataset, 'channel', IndEMG);

IndHEOGtmp = strfind(chanlabels, 'heog');
IndHEOG = find(not(cellfun('isempty', IndHEOGtmp)));
IndVEOGtmp = strfind(chanlabels, 'veog');
IndVEOG = find(not(cellfun('isempty', IndVEOGtmp)));
dataset_eog = pop_select(dataset, 'channel', [IndHEOG IndVEOG]);
        
% EEG channels
dataset = pop_select(dataset, 'nochannel', [IndHEOG IndVEOG IndEMG]);
     
%------------------------------------------------------------------------------------
        
% Re-reference to the biweight mean of the channels
% Because according to Biosemi the CMS electrode does not provide the full 80 dB CMRR
[avg_ch,~] = myBiweight(dataset.data');
repmat_temp = repmat(avg_ch,n_ch,1);
dataset.data = dataset.data - repmat_temp;
clear avg_ch
        
%------------------------------------------------------------------------------------
        
% Find bad channels 
[~,ch_std] = myBiweight(dataset.data);
bad_ch_ind_1 = myFindOutliers(ch_std);
ch_r_raw = corr(dataset.data');
ch_r_raw = sort(ch_r_raw);
ch_r = mean(ch_r_raw(end-4:end-1,:)); % top 4 correlation coefficients excluding self-correlation
bad_ch_ind_2 = myFindOutliers(ch_r);
% remove and interpolate bad channels
bad_ch = unique([bad_ch_ind_1,bad_ch_ind_2]);
eegplot(dataset.data)
disp('done')





%
%disp('!! FIRST STOP HERE. Add more bad channels.');

% !! FIRST STOP HERE.
% ADD MORE BAD CHANNELS INTO VARIABLE bad_ch
%bad_ch = [6];
if ~isempty(bad_ch)
    dataset = eeg_interp(dataset, bad_ch, 'spherical'); % REMOVING BAD CHANNELS HERE
end
%bad_channels = bad_channels + length(bad_ch); % the number of bad channels per subject
%eegplot(dataset.data)
%        
% EPOCH DATA

dataset = eeg_regepochs( dataset, 'limits', [0 epoch], 'rmbase', NaN, 'recurrence', epoch);
dataset = eeg_checkset( dataset );
dataset_eog = eeg_regepochs( dataset_eog, 'limits', [0 epoch], 'rmbase', NaN, 'recurrence', epoch);
dataset_eog = eeg_checkset( dataset_eog );
dataset_emg = eeg_regepochs( dataset_emg, 'limits', [0 epoch], 'rmbase', NaN, 'recurrence', epoch);
dataset_emg = eeg_checkset( dataset_emg );
       
% Remove trials that might contain artifacts that are too big for ICA decomposition
        
% Max amplitude difference
amp_diffs = zeros(size(dataset.data,1),size(dataset.data,3));
for iChan = 1:size(dataset.data,1)
    for itrial = 1:size(dataset.data,3)
        amp_diffs(iChan,itrial) = max(dataset.data(iChan,:,itrial)) - min(dataset.data(iChan,:,itrial));
    end
end
[epoch_amp_d,~] = myBiweight(amp_diffs');
% Epoch variance or the mean GFP
epoch_GFP = mean(squeeze(std(dataset.data,0,2)));
% Epoch's mean deviation from channel means.
[means,~] = myBiweight(dataset.data(:,:)); % channel mean for all epochs
epoch_m_dev = zeros(1,size(dataset.data,3));
for itrial = 1:size(dataset.data,3)
    epoch_m_dev(itrial) = mean(abs(squeeze(mean(dataset.data(:,:,itrial),2))' - means));
end

% Find the bad EPOCHS
Rej_ep_amp_d = myFindOutliers(epoch_amp_d);
Rej_ep_GFP = myFindOutliers(epoch_GFP);
Rej_ep_mdev = myFindOutliers(epoch_m_dev);
eegplot(dataset.data)
RRej_epoch = unique([Rej_ep_amp_d Rej_ep_GFP Rej_ep_mdev]);
disp('!! STOP HERE. Add more Rej_epoch.');

%% !! STOP HERE
% Add more bad (=epochs) into variable Rej_epoch_manual

%% NOW REMOVING BAD EPOCHS
Rej_epoch_manual = [1, 51, 75, 76, 136, 137, 150, 151, 152, 226, 301, 303, 376, 380, 381, 428, 434, 451, 508];
clear RRej_epoch;
%Rej_epoch = unique([Rej_ep_amp_d Rej_ep_GFP Rej_ep_mdev Rej_epoch_manual]);
Rej_epoch = unique([Rej_epoch_manual]);
dataset = pop_select(dataset,'notrial',Rej_epoch);
dataset_eog = pop_select(dataset_eog,'notrial',Rej_epoch);
dataset_emg = pop_select(dataset_emg,'notrial',Rej_epoch);
bad_trials = bad_trials + length(Rej_epoch);

disp('REMOVING BAD EPOCHS: Done');
%% RUN ICA    
%dataset = pop_saveset( dataset, 'dataset_before_ica', 'C:\Users\user\Documents\Marie\Prepocessed_Data\temp\');
%pop_saveset( dataset)
 
%------------------------------------   ------------------------------------------------
% Perform ICA - SOBI
%------------------------------------------------------------------------------------
 EEG = pop_runica(dataset, 'icatype', 'sobi', 'dataset',1, 'options',{});
 if isempty(EEG.icaact)
     disp('EEG.icaact not present. Recomputed from data.');
     if length(size(EEG.data))==3
         EEG.icaact = reshape(EEG.icaweights*EEG.icasphere*reshape(EEG.data,[size(EEG.data,1)...
                        size(EEG.data,2)*size(EEG.data,3)]),[size(EEG.data,1) size(EEG.data,2) size(EEG.data,3)]);
     else
         EEG.icaact = EEG.icaweights*EEG.icasphere*EEG.data;
     end
 end
            
 ncomp = length(EEG.icawinv); % number of components
            
 % Eye blinks and saccades detection by correlation with VEOG and HEOG
 VEOG = dataset_eog.data(2,:,:);
 VEOG = VEOG(:);
 HEOG = dataset_eog.data(1,:,:);
 HEOG = HEOG(:);
 ICs = EEG.icaact(:,:)';
 for ic = 1:size(ICs,2)
     corr_V(ic) = corr(ICs(:,ic),VEOG);
     corr_H(ic) = corr(ICs(:,ic),HEOG);
 end
 rej_V = myFindOutliers(corr_V);
 rej_H = myFindOutliers(corr_H);
 Rej_ic_eog = unique([rej_V,rej_H]);     % ICs containing blinks
 clear rej_V rej_H % free some memory
            
 % ICs with generics discontinutiy of spatial features
 topography = EEG.icawinv';  % topography of the IC weigths
 channel = EEG.chanlocs(EEG.icachansind');
 xpos=[channel.X];ypos=[channel.Y];zpos=[channel.Z];
 pos=[xpos',ypos',zpos'];
 gen_disc = zeros(1,size(ICs,2)); % generic discontinuity
 for ic = 1:ncomp
     aux = [];
     for el = 1:length(channel)-1
                    
          P_el = pos(el,:); %position of current electrode
          d = pos - repmat(P_el,length(channel),1);
          dist = sqrt(sum((d.*d),2));
                    
          [y,I] = sort(dist);
          rep_ch = I(2:11); % the 10 nearest channels to el
          weight_ch = exp(-y(2:11)); % respective weights, computed wrt distance
                    
          aux = [aux abs(topography(ic,el)-mean(weight_ch.*topography(ic,rep_ch)'))];
          % difference between el and the average of 10 neighbor el
          % weighted according to weight
      end
      gen_disc(ic)=max(aux);
 end
 Rej_ic_gd = myFindOutliers(gen_disc); % ICs containing generic discontinuities
 clear channel aux pos topography % free some memory
            
 % Muscle activity usually has low autocorrelation of time course
 ncorrint =round(25/(1000/EEG.srate)); % number of samples for 25 ms lag
 for k = 1:ncomp
      y = EEG.icaact(k,:,:);
      yy = xcorr(mean(y,3),ncorrint,'coeff');
      autocorr(k) = yy(1);
 end
 Rej_muscle = myFindOutliers(autocorr);

 %pop_saveset(EEG)
 pop_selectcomps(EEG, [1:63] );
 %dataset = pop_saveset( dataset, 'dataset_ica_run_but_not_rejected', 'C:\Users\user\Documents\Marie\Prepocessed_Data\temp\')
 
 
 %% Drop the ICs
 % !!! PUT ALL IDs IN THE DROPLIST VARIABLE BELOW
 % then do the remaining preprocessing and SAVE
 
 %droplist = unique([Rej_ic_eog Rej_muscle Rej_ic_gd]); % save the number of the components droped
 droplist = [10, 16, 36, 49, 51, 52]; % save the number of the components dropped
 if ~isempty(droplist)
      EEG = pop_subcomp( EEG, droplist, 0); %drop components
 end
      EEG = eeg_checkset( EEG );
        
 clear dataset
% eegplot(EEG.data)

%  Now save this dataset. 
%EEG = pop_saveset(EEG, 'dataset_after_ica_automatic', 'C:\Users\user\Documents\Marie\Prepocessed_Data\temp\');
%pop_saveset(EEG);

% Check if still have some artifacts after IC -- focus on each channel of each trial


EEGtmp = EEG; % local copy
for itrial = 1:EEG.trials
     EEGtmp.data = EEG.data(:,:,itrial);
            
     % Mean diff value
     [mean_diff,~]=myBiweight(diff(EEGtmp.data,[],2));
     % Variance of the channels
     [~,chan_std]=myBiweight(EEGtmp.data);
            
     % Find the outlier channels
     Rej_mean_diff = myFindOutliers(mean_diff);
     Rej_chan_std = myFindOutliers(chan_std);
     bad_ch_trial = intersect(Rej_mean_diff, Rej_chan_std); % 18/07/2017
     if ~isempty(bad_ch_trial)
          EEGtmp = eeg_interp(EEGtmp, bad_ch_trial, 'spherical');
     end
     % add the data to the dataset
     EEG.data(:,:,itrial) = EEGtmp.data;
            
     % count the number of bad channel in each trial
     trial_bad_ch = trial_bad_ch + length(bad_ch_trial);
            
end
clear EEGtmp % clear some memory
eegplot(EEG.data)        
 
% save dataset again
%pop_saveset(EEG)




%% CLEAN SOME MORE EPOCHS IF NEEDED
% PUT THE EPOCHS YOU WANT TO REMOVE INTO VARIABLE d BELOW
% !! DO NOT run this section if you don't need to clean any more epochs
d = [1,2,];
EEG = pop_select(EEG,'notrial',d);
clear d;
pop_saveset(EEG)




%% Second ICA
% !!! DON'T RUN THIS SECTION IF YOU DON'T WANT TO DO THE SECOND ICA

 EEG = pop_runica(EEG, 'icatype', 'sobi', 'EEG',1, 'options',{});
 %EEG = pop_runica(EEG, 'icatype', 'sobi', 'dataset',1, 'options',{});

 if isempty(EEG.icaact)
     disp('EEG.icaact not present. Recomputed from data.');
     if length(size(EEG.data))==3
         EEG.icaact = reshape(EEG.icaweights*EEG.icasphere*reshape(EEG.data,[size(EEG.data,1)...
                        size(EEG.data,2)*size(EEG.data,3)]),[size(EEG.data,1) size(EEG.data,2) size(EEG.data,3)]);
     else
         EEG.icaact = EEG.icaweights*EEG.icasphere*EEG.data;
     end
 end           
 ncomp = length(EEG.icawinv); % number of components
 pop_selectcomps(EEG, [1:66] );
 
 %% Drop ICs
 droplist = [48]; % save the number of the components dropped
 if ~isempty(droplist)
      EEG = pop_subcomp( EEG, droplist, 0); %drop components
 end
      EEG = eeg_checkset( EEG );
 
 %pop_saveset(EEG)
 eegplot(EEG.data)
 
 %% CLEAN SOME MORE EPOCHS IF NEEDED
% PUT THE EPOCHS YOU WANT TO REMOVE INTO VARIABLE d BELOW

% !! DO NOT run this section if you don't need to clean any more epochs

dd = [25]; % put epochs to reject here
EEG = pop_select(EEG,'notrial',dd);
clear dd;
pop_saveset(EEG)
 
 %% IF WANT TO DO ANOTHER IC WITH RUNICA
 % load the last set into gui
 % do stuff
 EEG = pop_loadset('filename','set_2OTPH10_afterica.set','filepath','C:\\Users\\user\\Documents\\Marie\\Analysis_2019\\1_Preprocessing\\Sets\\');
 EEG = eeg_checkset( EEG );
disp('SET LOADED');

eegplot(EEG.data)


%% FINAL STEPS AND SAVE DATA
% Prepare the data to be stuck together by using inverse hanning window
xtmp = EEG.data(:,:);
for ichan = 1:size(xtmp,1)
     xtmp(ichan,:) = hann_intersection(xtmp(ichan,:), EEG.srate, 1/4, EEG.srate*epoch);
end
        
% copy data and check the set, the set check is already epoched again but it's okay since already hanninged
EEG.data = xtmp;
        
% reference to the average
EEG = pop_reref( EEG, []);
EEG = eeg_checkset( EEG );

% save the data
save(strcat(SaveDir,'\',char(Subject_pool{iSubject}(1:end-5)),'_preprocess.mat'),'EEG',...
            'bad_trials','bad_channels','trial_bad_ch','dataset_emg');
        
        
disp('WOOHOO ANOTHER RECORDING CLEAN! NOW TAKE A BREAK ;)');
