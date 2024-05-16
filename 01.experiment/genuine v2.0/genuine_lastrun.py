#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""
This experiment was created using PsychoPy3 Experiment Builder (v2022.1.4),
    on Fri Mar  8 13:48:28 2024
If you publish work using this script the most relevant publication is:

    Peirce J, Gray JR, Simpson S, MacAskill M, Höchenberger R, Sogo H, Kastman E, Lindeløv JK. (2019) 
        PsychoPy2: Experiments in behavior made easy Behav Res 51: 195. 
        https://doi.org/10.3758/s13428-018-01193-y

"""

from psychopy import locale_setup
from psychopy import prefs
from psychopy import sound, gui, visual, core, data, event, logging, clock, colors, layout
from psychopy.constants import (NOT_STARTED, STARTED, PLAYING, PAUSED,
                                STOPPED, FINISHED, PRESSED, RELEASED, FOREVER)

import numpy as np  # whole numpy lib is available, prepend 'np.'
from numpy import (sin, cos, tan, log, log10, pi, average,
                   sqrt, std, deg2rad, rad2deg, linspace, asarray)
from numpy.random import random, randint, normal, shuffle, choice as randchoice
import os  # handy system and path functions
import sys  # to get file system encoding

import psychopy.iohub as io
from psychopy.hardware import keyboard



# Ensure that relative paths start from the same directory as this script
_thisDir = os.path.dirname(os.path.abspath(__file__))
os.chdir(_thisDir)
# Store info about the experiment session
psychopyVersion = '2022.1.4'
expName = 'genuine'  # from the Builder filename that created this script
expInfo = {
    'participant': '',
    'session': '001',
    'name': '',
    'eta': '',
    'gender': '',
    'education': '',
}
dlg = gui.DlgFromDict(dictionary=expInfo, sortKeys=False, title=expName)
if dlg.OK == False:
    core.quit()  # user pressed cancel
expInfo['date'] = data.getDateStr()  # add a simple timestamp
expInfo['expName'] = expName
expInfo['psychopyVersion'] = psychopyVersion

# Data file name stem = absolute path + name; later add .psyexp, .csv, .log, etc
filename = _thisDir + os.sep + u'data/%s_%s_%s' % (expInfo['participant'], expName, expInfo['date'])

# An ExperimentHandler isn't essential but helps with data saving
thisExp = data.ExperimentHandler(name=expName, version='',
    extraInfo=expInfo, runtimeInfo=None,
    originPath='/Users/thomasquettier/Library/Mobile Documents/com~apple~CloudDocs/01.WORK/10.Genuine/01.experiment/genuine v2.0/genuine_lastrun.py',
    savePickle=True, saveWideText=True,
    dataFileName=filename)
# save a log file for detail verbose info
logFile = logging.LogFile(filename+'.log', level=logging.EXP)
logging.console.setLevel(logging.WARNING)  # this outputs to the screen, not a file

endExpNow = False  # flag for 'escape' or other condition => quit the exp
frameTolerance = 0.001  # how close to onset before 'same' frame

# Start Code - component code to be run after the window creation

# Setup the Window
win = visual.Window(
    size=[1680, 1050], fullscr=True, screen=0, 
    winType='pyglet', allowGUI=False, allowStencil=False,
    monitor='testMonitor', color=[-1,-1,-1], colorSpace='rgb',
    blendMode='avg', useFBO=True, 
    units='height')
# store frame rate of monitor if we can measure it
expInfo['frameRate'] = win.getActualFrameRate()
if expInfo['frameRate'] != None:
    frameDur = 1.0 / round(expInfo['frameRate'])
else:
    frameDur = 1.0 / 60.0  # could not measure, so guess
# Setup ioHub
ioConfig = {}

# Setup iohub keyboard
ioConfig['Keyboard'] = dict(use_keymap='psychopy')

ioSession = '1'
if 'session' in expInfo:
    ioSession = str(expInfo['session'])
ioServer = io.launchHubServer(window=win, **ioConfig)
eyetracker = None

# create a default keyboard (e.g. to check for escape)
defaultKeyboard = keyboard.Keyboard(backend='iohub')

# Initialize components for Routine "WELCOME"
WELCOMEClock = core.Clock()
original_size = [503,236]
button_size = [x * 0.5 for x in original_size]
welcome_txt = visual.TextStim(win=win, name='welcome_txt',
    text='Premere il pulsante blu per iniziare',
    font='Open Sans',
    pos=(0, 0), height=0.05, wrapWidth=None, ori=0.0, 
    color='white', colorSpace='rgb', opacity=None, 
    languageStyle='LTR',
    depth=-1.0);
welcome_img = visual.ImageStim(
    win=win,
    name='welcome_img', units='pix', 
    image='interface/button_avanti.png', mask=None, anchor='center',
    ori=0.0, pos=(400, -250), size=(button_size[0], button_size[1]),
    color=[1,1,1], colorSpace='rgb', opacity=None,
    flipHoriz=False, flipVert=False,
    texRes=128.0, interpolate=True, depth=-2.0)
welcome_ms = event.Mouse(win=win)
x, y = [None, None]
welcome_ms.mouseClock = core.Clock()

# Initialize components for Routine "BLACKSCREEN"
BLACKSCREENClock = core.Clock()
blackscreen_txt = visual.TextStim(win=win, name='blackscreen_txt',
    text=None,
    font='Open Sans',
    pos=(0, 0), height=0.05, wrapWidth=None, ori=0.0, 
    color='white', colorSpace='rgb', opacity=None, 
    languageStyle='LTR',
    depth=-1.0);

# Initialize components for Routine "TRIAL"
TRIALClock = core.Clock()

# Initialize components for Routine "GESINO"
GESINOClock = core.Clock()

button_pos2 = list([400, -400])
gesino_txt = visual.TextStim(win=win, name='gesino_txt',
    text="L'emozione mostrata è genuina?\n\nv = vero\n\nn = falso",
    font='Open Sans',
    pos=(0, 0), height=0.05, wrapWidth=None, ori=0.0, 
    color='white', colorSpace='rgb', opacity=None, 
    languageStyle='LTR',
    depth=-1.0);
gesino_kb = keyboard.Keyboard()

# Initialize components for Routine "GENUINE"
GENUINEClock = core.Clock()
genuine_txt = visual.TextStim(win=win, name='genuine_txt',
    text="L'emozione mostrata era genuina?",
    font='Open Sans',
    pos=(0, 0.2), height=0.05, wrapWidth=None, ori=0.0, 
    color='white', colorSpace='rgb', opacity=None, 
    languageStyle='LTR',
    depth=0.0);
genuine_slider = visual.Slider(win=win, name='genuine_slider',
    startValue=0, size=(0.95, 0.05), pos=(0, 0), units=None,
    labels=["-7","-6","-5","-4","-3","-2","-1","0","1","2","3","4","5","6","7"], ticks=(-7, -6,-5,-4,-3,-2,-1,0,1,2,3,4,5,6,7), granularity=1.0,
    style='rating', styleTweaks=('labels45',), opacity=None,
    labelColor='LightGray', markerColor='Red', lineColor='White', colorSpace='rgb',
    font='Open Sans', labelHeight=0.05,
    flip=False, ori=0.0, depth=-1, readOnly=False)
genuine_img = visual.ImageStim(
    win=win,
    name='genuine_img', units='pix', 
    image='interface/button_avanti.png', mask=None, anchor='center',
    ori=0.0, pos=(400, -250), size=(button_size[0], button_size[1]),
    color=[1,1,1], colorSpace='rgb', opacity=None,
    flipHoriz=False, flipVert=False,
    texRes=128.0, interpolate=True, depth=-2.0)
genuine_ms = event.Mouse(win=win)
x, y = [None, None]
genuine_ms.mouseClock = core.Clock()
genuine_txt2 = visual.TextStim(win=win, name='genuine_txt2',
    text='-7 = Per niente genuina,  7 = Assolutamente genuina',
    font='Open Sans',
    pos=(0, -0.4), height=0.03, wrapWidth=None, ori=0.0, 
    color='white', colorSpace='rgb', opacity=None, 
    languageStyle='LTR',
    depth=-4.0);

# Initialize components for Routine "EMOTION"
EMOTIONClock = core.Clock()

button_pos = list([400, 0, -400])
emotion_txt = visual.TextStim(win=win, name='emotion_txt',
    text="L'emozione mostrata era:",
    font='Open Sans',
    pos=(0, 0.2), height=0.05, wrapWidth=None, ori=0.0, 
    color='white', colorSpace='rgb', opacity=None, 
    languageStyle='LTR',
    depth=-1.0);
fear = visual.ImageStim(
    win=win,
    name='fear', units='pix', 
    image='interface/button_fear.png', mask=None, anchor='center',
    ori=0.0, pos=[0,0], size=(button_size[0], button_size[1]),
    color=[1,1,1], colorSpace='rgb', opacity=None,
    flipHoriz=False, flipVert=False,
    texRes=128.0, interpolate=True, depth=-2.0)
anger = visual.ImageStim(
    win=win,
    name='anger', units='pix', 
    image='interface/button_anger.png', mask=None, anchor='center',
    ori=0.0, pos=[0,0], size=(button_size[0], button_size[1]),
    color=[1,1,1], colorSpace='rgb', opacity=None,
    flipHoriz=False, flipVert=False,
    texRes=128.0, interpolate=True, depth=-3.0)
happiness = visual.ImageStim(
    win=win,
    name='happiness', units='pix', 
    image='interface/button_gioia.png', mask=None, anchor='center',
    ori=0.0, pos=[0,0], size=(button_size[0], button_size[1]),
    color=[1,1,1], colorSpace='rgb', opacity=None,
    flipHoriz=False, flipVert=False,
    texRes=128.0, interpolate=True, depth=-4.0)
emotion_ms = event.Mouse(win=win)
x, y = [None, None]
emotion_ms.mouseClock = core.Clock()

# Initialize components for Routine "INTENSITY"
INTENSITYClock = core.Clock()
intensity_txt = visual.TextStim(win=win, name='intensity_txt',
    text="L'emozione mostrata era intensa?",
    font='Open Sans',
    pos=(0, 0.2), height=0.05, wrapWidth=None, ori=0.0, 
    color='white', colorSpace='rgb', opacity=None, 
    languageStyle='LTR',
    depth=0.0);
intensity_slider = visual.Slider(win=win, name='intensity_slider',
    startValue=0, size=(0.75, 0.05), pos=(0, 0), units=None,
    labels=["0","1","2","3","4","5","6","7","8","9"], ticks=(0,1,2,3,4,5,6,7,8,9), granularity=1.0,
    style='rating', styleTweaks=('labels45',), opacity=None,
    labelColor='LightGray', markerColor='Red', lineColor='White', colorSpace='rgb',
    font='Open Sans', labelHeight=0.05,
    flip=False, ori=0.0, depth=-1, readOnly=False)
intensity_img = visual.ImageStim(
    win=win,
    name='intensity_img', units='pix', 
    image='interface/button_avanti.png', mask=None, anchor='center',
    ori=0.0, pos=(400, -250), size=(button_size[0], button_size[1]),
    color=[1,1,1], colorSpace='rgb', opacity=None,
    flipHoriz=False, flipVert=False,
    texRes=128.0, interpolate=True, depth=-2.0)
intensity_ms = event.Mouse(win=win)
x, y = [None, None]
intensity_ms.mouseClock = core.Clock()
intensity_txt2 = visual.TextStim(win=win, name='intensity_txt2',
    text='0 = Per niente intensa, 9 = Molto intensa',
    font='Open Sans',
    pos=(0, -0.4), height=0.03, wrapWidth=None, ori=0.0, 
    color='white', colorSpace='rgb', opacity=None, 
    languageStyle='LTR',
    depth=-4.0);

# Initialize components for Routine "END"
ENDClock = core.Clock()
end_txt = visual.TextStim(win=win, name='end_txt',
    text="L'esperimento è finito.\n\nGrazie",
    font='Open Sans',
    pos=(0, 0), height=0.05, wrapWidth=None, ori=0.0, 
    color='white', colorSpace='rgb', opacity=None, 
    languageStyle='LTR',
    depth=0.0);
end_kb = keyboard.Keyboard()

# Create some handy timers
globalClock = core.Clock()  # to track the time since experiment started
routineTimer = core.CountdownTimer()  # to track time remaining of each (non-slip) routine 

# ------Prepare to start Routine "WELCOME"-------
continueRoutine = True
# update component parameters for each repeat
# setup some python lists for storing info about the welcome_ms
welcome_ms.clicked_name = []
gotValidClick = False  # until a click is received
# keep track of which components have finished
WELCOMEComponents = [welcome_txt, welcome_img, welcome_ms]
for thisComponent in WELCOMEComponents:
    thisComponent.tStart = None
    thisComponent.tStop = None
    thisComponent.tStartRefresh = None
    thisComponent.tStopRefresh = None
    if hasattr(thisComponent, 'status'):
        thisComponent.status = NOT_STARTED
# reset timers
t = 0
_timeToFirstFrame = win.getFutureFlipTime(clock="now")
WELCOMEClock.reset(-_timeToFirstFrame)  # t0 is time of first possible flip
frameN = -1

# -------Run Routine "WELCOME"-------
while continueRoutine:
    # get current time
    t = WELCOMEClock.getTime()
    tThisFlip = win.getFutureFlipTime(clock=WELCOMEClock)
    tThisFlipGlobal = win.getFutureFlipTime(clock=None)
    frameN = frameN + 1  # number of completed frames (so 0 is the first frame)
    # update/draw components on each frame
    
    # *welcome_txt* updates
    if welcome_txt.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
        # keep track of start time/frame for later
        welcome_txt.frameNStart = frameN  # exact frame index
        welcome_txt.tStart = t  # local t and not account for scr refresh
        welcome_txt.tStartRefresh = tThisFlipGlobal  # on global time
        win.timeOnFlip(welcome_txt, 'tStartRefresh')  # time at next scr refresh
        welcome_txt.setAutoDraw(True)
    
    # *welcome_img* updates
    if welcome_img.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
        # keep track of start time/frame for later
        welcome_img.frameNStart = frameN  # exact frame index
        welcome_img.tStart = t  # local t and not account for scr refresh
        welcome_img.tStartRefresh = tThisFlipGlobal  # on global time
        win.timeOnFlip(welcome_img, 'tStartRefresh')  # time at next scr refresh
        welcome_img.setAutoDraw(True)
    # *welcome_ms* updates
    if welcome_ms.status == NOT_STARTED and t >= 0.0-frameTolerance:
        # keep track of start time/frame for later
        welcome_ms.frameNStart = frameN  # exact frame index
        welcome_ms.tStart = t  # local t and not account for scr refresh
        welcome_ms.tStartRefresh = tThisFlipGlobal  # on global time
        win.timeOnFlip(welcome_ms, 'tStartRefresh')  # time at next scr refresh
        welcome_ms.status = STARTED
        welcome_ms.mouseClock.reset()
        prevButtonState = welcome_ms.getPressed()  # if button is down already this ISN'T a new click
    if welcome_ms.status == STARTED:  # only update if started and not finished!
        buttons = welcome_ms.getPressed()
        if buttons != prevButtonState:  # button state changed?
            prevButtonState = buttons
            if sum(buttons) > 0:  # state changed to a new click
                # check if the mouse was inside our 'clickable' objects
                gotValidClick = False
                try:
                    iter(welcome_img)
                    clickableList = welcome_img
                except:
                    clickableList = [welcome_img]
                for obj in clickableList:
                    if obj.contains(welcome_ms):
                        gotValidClick = True
                        welcome_ms.clicked_name.append(obj.name)
                if gotValidClick:  
                    continueRoutine = False  # abort routine on response
    
    # check for quit (typically the Esc key)
    if endExpNow or defaultKeyboard.getKeys(keyList=["escape"]):
        core.quit()
    
    # check if all components have finished
    if not continueRoutine:  # a component has requested a forced-end of Routine
        break
    continueRoutine = False  # will revert to True if at least one component still running
    for thisComponent in WELCOMEComponents:
        if hasattr(thisComponent, "status") and thisComponent.status != FINISHED:
            continueRoutine = True
            break  # at least one component has not yet finished
    
    # refresh the screen
    if continueRoutine:  # don't flip if this routine is over or we'll get a blank screen
        win.flip()

# -------Ending Routine "WELCOME"-------
for thisComponent in WELCOMEComponents:
    if hasattr(thisComponent, "setAutoDraw"):
        thisComponent.setAutoDraw(False)
# store data for thisExp (ExperimentHandler)
thisExp.nextEntry()
# the Routine "WELCOME" was not non-slip safe, so reset the non-slip timer
routineTimer.reset()

# set up handler to look after randomisation of conditions etc
Trials_loop = data.TrialHandler(nReps=1.0, method='random', 
    extraInfo=expInfo, originPath=-1,
    trialList=data.importConditions('PEDFE/edited/PEDFE.xlsx'),
    seed=None, name='Trials_loop')
thisExp.addLoop(Trials_loop)  # add the loop to the experiment
thisTrials_loop = Trials_loop.trialList[0]  # so we can initialise stimuli with some values
# abbreviate parameter names if possible (e.g. rgb = thisTrials_loop.rgb)
if thisTrials_loop != None:
    for paramName in thisTrials_loop:
        exec('{} = thisTrials_loop[paramName]'.format(paramName))

for thisTrials_loop in Trials_loop:
    currentLoop = Trials_loop
    # abbreviate parameter names if possible (e.g. rgb = thisTrials_loop.rgb)
    if thisTrials_loop != None:
        for paramName in thisTrials_loop:
            exec('{} = thisTrials_loop[paramName]'.format(paramName))
    
    # ------Prepare to start Routine "BLACKSCREEN"-------
    continueRoutine = True
    routineTimer.add(2.000000)
    # update component parameters for each repeat
    win.mouseVisible = False
    
    
    # keep track of which components have finished
    BLACKSCREENComponents = [blackscreen_txt]
    for thisComponent in BLACKSCREENComponents:
        thisComponent.tStart = None
        thisComponent.tStop = None
        thisComponent.tStartRefresh = None
        thisComponent.tStopRefresh = None
        if hasattr(thisComponent, 'status'):
            thisComponent.status = NOT_STARTED
    # reset timers
    t = 0
    _timeToFirstFrame = win.getFutureFlipTime(clock="now")
    BLACKSCREENClock.reset(-_timeToFirstFrame)  # t0 is time of first possible flip
    frameN = -1
    
    # -------Run Routine "BLACKSCREEN"-------
    while continueRoutine and routineTimer.getTime() > 0:
        # get current time
        t = BLACKSCREENClock.getTime()
        tThisFlip = win.getFutureFlipTime(clock=BLACKSCREENClock)
        tThisFlipGlobal = win.getFutureFlipTime(clock=None)
        frameN = frameN + 1  # number of completed frames (so 0 is the first frame)
        # update/draw components on each frame
        
        # *blackscreen_txt* updates
        if blackscreen_txt.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            blackscreen_txt.frameNStart = frameN  # exact frame index
            blackscreen_txt.tStart = t  # local t and not account for scr refresh
            blackscreen_txt.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(blackscreen_txt, 'tStartRefresh')  # time at next scr refresh
            blackscreen_txt.setAutoDraw(True)
        if blackscreen_txt.status == STARTED:
            # is it time to stop? (based on global clock, using actual start)
            if tThisFlipGlobal > blackscreen_txt.tStartRefresh + 2-frameTolerance:
                # keep track of stop time/frame for later
                blackscreen_txt.tStop = t  # not accounting for scr refresh
                blackscreen_txt.frameNStop = frameN  # exact frame index
                win.timeOnFlip(blackscreen_txt, 'tStopRefresh')  # time at next scr refresh
                blackscreen_txt.setAutoDraw(False)
        
        # check for quit (typically the Esc key)
        if endExpNow or defaultKeyboard.getKeys(keyList=["escape"]):
            core.quit()
        
        # check if all components have finished
        if not continueRoutine:  # a component has requested a forced-end of Routine
            break
        continueRoutine = False  # will revert to True if at least one component still running
        for thisComponent in BLACKSCREENComponents:
            if hasattr(thisComponent, "status") and thisComponent.status != FINISHED:
                continueRoutine = True
                break  # at least one component has not yet finished
        
        # refresh the screen
        if continueRoutine:  # don't flip if this routine is over or we'll get a blank screen
            win.flip()
    
    # -------Ending Routine "BLACKSCREEN"-------
    for thisComponent in BLACKSCREENComponents:
        if hasattr(thisComponent, "setAutoDraw"):
            thisComponent.setAutoDraw(False)
    
    # ------Prepare to start Routine "TRIAL"-------
    continueRoutine = True
    # update component parameters for each repeat
    win.mouseVisible = False
    trial_mp4 = visual.MovieStim3(
        win=win, name='trial_mp4', units='pix',
        noAudio = True,
        filename=os.path.join("PEDFE/edited",file),
        ori=0.0, pos=(0, 0), opacity=None,
        loop=False, anchor='center',
        size=(h*2,w*2),
        depth=-1.0,
        )
    # keep track of which components have finished
    TRIALComponents = [trial_mp4]
    for thisComponent in TRIALComponents:
        thisComponent.tStart = None
        thisComponent.tStop = None
        thisComponent.tStartRefresh = None
        thisComponent.tStopRefresh = None
        if hasattr(thisComponent, 'status'):
            thisComponent.status = NOT_STARTED
    # reset timers
    t = 0
    _timeToFirstFrame = win.getFutureFlipTime(clock="now")
    TRIALClock.reset(-_timeToFirstFrame)  # t0 is time of first possible flip
    frameN = -1
    
    # -------Run Routine "TRIAL"-------
    while continueRoutine:
        # get current time
        t = TRIALClock.getTime()
        tThisFlip = win.getFutureFlipTime(clock=TRIALClock)
        tThisFlipGlobal = win.getFutureFlipTime(clock=None)
        frameN = frameN + 1  # number of completed frames (so 0 is the first frame)
        # update/draw components on each frame
        
        # *trial_mp4* updates
        if trial_mp4.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            trial_mp4.frameNStart = frameN  # exact frame index
            trial_mp4.tStart = t  # local t and not account for scr refresh
            trial_mp4.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(trial_mp4, 'tStartRefresh')  # time at next scr refresh
            trial_mp4.setAutoDraw(True)
        
        # check for quit (typically the Esc key)
        if endExpNow or defaultKeyboard.getKeys(keyList=["escape"]):
            core.quit()
        
        # check if all components have finished
        if not continueRoutine:  # a component has requested a forced-end of Routine
            break
        continueRoutine = False  # will revert to True if at least one component still running
        for thisComponent in TRIALComponents:
            if hasattr(thisComponent, "status") and thisComponent.status != FINISHED:
                continueRoutine = True
                break  # at least one component has not yet finished
        
        # refresh the screen
        if continueRoutine:  # don't flip if this routine is over or we'll get a blank screen
            win.flip()
    
    # -------Ending Routine "TRIAL"-------
    for thisComponent in TRIALComponents:
        if hasattr(thisComponent, "setAutoDraw"):
            thisComponent.setAutoDraw(False)
    win.mouseVisible = True
    trial_mp4.stop()
    # the Routine "TRIAL" was not non-slip safe, so reset the non-slip timer
    routineTimer.reset()
    
    # ------Prepare to start Routine "GESINO"-------
    continueRoutine = True
    # update component parameters for each repeat
    np.random.shuffle(button_pos2)
    gesino_kb.keys = []
    gesino_kb.rt = []
    _gesino_kb_allKeys = []
    # keep track of which components have finished
    GESINOComponents = [gesino_txt, gesino_kb]
    for thisComponent in GESINOComponents:
        thisComponent.tStart = None
        thisComponent.tStop = None
        thisComponent.tStartRefresh = None
        thisComponent.tStopRefresh = None
        if hasattr(thisComponent, 'status'):
            thisComponent.status = NOT_STARTED
    # reset timers
    t = 0
    _timeToFirstFrame = win.getFutureFlipTime(clock="now")
    GESINOClock.reset(-_timeToFirstFrame)  # t0 is time of first possible flip
    frameN = -1
    
    # -------Run Routine "GESINO"-------
    while continueRoutine:
        # get current time
        t = GESINOClock.getTime()
        tThisFlip = win.getFutureFlipTime(clock=GESINOClock)
        tThisFlipGlobal = win.getFutureFlipTime(clock=None)
        frameN = frameN + 1  # number of completed frames (so 0 is the first frame)
        # update/draw components on each frame
        
        # *gesino_txt* updates
        if gesino_txt.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            gesino_txt.frameNStart = frameN  # exact frame index
            gesino_txt.tStart = t  # local t and not account for scr refresh
            gesino_txt.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(gesino_txt, 'tStartRefresh')  # time at next scr refresh
            gesino_txt.setAutoDraw(True)
        
        # *gesino_kb* updates
        waitOnFlip = False
        if gesino_kb.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            gesino_kb.frameNStart = frameN  # exact frame index
            gesino_kb.tStart = t  # local t and not account for scr refresh
            gesino_kb.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(gesino_kb, 'tStartRefresh')  # time at next scr refresh
            gesino_kb.status = STARTED
            # keyboard checking is just starting
            waitOnFlip = True
            win.callOnFlip(gesino_kb.clock.reset)  # t=0 on next screen flip
            win.callOnFlip(gesino_kb.clearEvents, eventType='keyboard')  # clear events on next screen flip
        if gesino_kb.status == STARTED and not waitOnFlip:
            theseKeys = gesino_kb.getKeys(keyList=['v','n'], waitRelease=False)
            _gesino_kb_allKeys.extend(theseKeys)
            if len(_gesino_kb_allKeys):
                gesino_kb.keys = _gesino_kb_allKeys[-1].name  # just the last key pressed
                gesino_kb.rt = _gesino_kb_allKeys[-1].rt
                # was this correct?
                if (gesino_kb.keys == str(genuine)) or (gesino_kb.keys == genuine):
                    gesino_kb.corr = 1
                else:
                    gesino_kb.corr = 0
                # a response ends the routine
                continueRoutine = False
        
        # check for quit (typically the Esc key)
        if endExpNow or defaultKeyboard.getKeys(keyList=["escape"]):
            core.quit()
        
        # check if all components have finished
        if not continueRoutine:  # a component has requested a forced-end of Routine
            break
        continueRoutine = False  # will revert to True if at least one component still running
        for thisComponent in GESINOComponents:
            if hasattr(thisComponent, "status") and thisComponent.status != FINISHED:
                continueRoutine = True
                break  # at least one component has not yet finished
        
        # refresh the screen
        if continueRoutine:  # don't flip if this routine is over or we'll get a blank screen
            win.flip()
    
    # -------Ending Routine "GESINO"-------
    for thisComponent in GESINOComponents:
        if hasattr(thisComponent, "setAutoDraw"):
            thisComponent.setAutoDraw(False)
    # check responses
    if gesino_kb.keys in ['', [], None]:  # No response was made
        gesino_kb.keys = None
        # was no response the correct answer?!
        if str(genuine).lower() == 'none':
           gesino_kb.corr = 1;  # correct non-response
        else:
           gesino_kb.corr = 0;  # failed to respond (incorrectly)
    # store data for Trials_loop (TrialHandler)
    Trials_loop.addData('gesino_kb.keys',gesino_kb.keys)
    Trials_loop.addData('gesino_kb.corr', gesino_kb.corr)
    if gesino_kb.keys != None:  # we had a response
        Trials_loop.addData('gesino_kb.rt', gesino_kb.rt)
    Trials_loop.addData('gesino_kb.started', gesino_kb.tStartRefresh)
    Trials_loop.addData('gesino_kb.stopped', gesino_kb.tStopRefresh)
    # the Routine "GESINO" was not non-slip safe, so reset the non-slip timer
    routineTimer.reset()
    
    # ------Prepare to start Routine "GENUINE"-------
    continueRoutine = True
    # update component parameters for each repeat
    genuine_slider.reset()
    # setup some python lists for storing info about the genuine_ms
    genuine_ms.clicked_name = []
    gotValidClick = False  # until a click is received
    # keep track of which components have finished
    GENUINEComponents = [genuine_txt, genuine_slider, genuine_img, genuine_ms, genuine_txt2]
    for thisComponent in GENUINEComponents:
        thisComponent.tStart = None
        thisComponent.tStop = None
        thisComponent.tStartRefresh = None
        thisComponent.tStopRefresh = None
        if hasattr(thisComponent, 'status'):
            thisComponent.status = NOT_STARTED
    # reset timers
    t = 0
    _timeToFirstFrame = win.getFutureFlipTime(clock="now")
    GENUINEClock.reset(-_timeToFirstFrame)  # t0 is time of first possible flip
    frameN = -1
    
    # -------Run Routine "GENUINE"-------
    while continueRoutine:
        # get current time
        t = GENUINEClock.getTime()
        tThisFlip = win.getFutureFlipTime(clock=GENUINEClock)
        tThisFlipGlobal = win.getFutureFlipTime(clock=None)
        frameN = frameN + 1  # number of completed frames (so 0 is the first frame)
        # update/draw components on each frame
        
        # *genuine_txt* updates
        if genuine_txt.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            genuine_txt.frameNStart = frameN  # exact frame index
            genuine_txt.tStart = t  # local t and not account for scr refresh
            genuine_txt.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(genuine_txt, 'tStartRefresh')  # time at next scr refresh
            genuine_txt.setAutoDraw(True)
        
        # *genuine_slider* updates
        if genuine_slider.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            genuine_slider.frameNStart = frameN  # exact frame index
            genuine_slider.tStart = t  # local t and not account for scr refresh
            genuine_slider.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(genuine_slider, 'tStartRefresh')  # time at next scr refresh
            genuine_slider.setAutoDraw(True)
        
        # *genuine_img* updates
        if genuine_img.status == NOT_STARTED and genuine_slider.rating:
            # keep track of start time/frame for later
            genuine_img.frameNStart = frameN  # exact frame index
            genuine_img.tStart = t  # local t and not account for scr refresh
            genuine_img.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(genuine_img, 'tStartRefresh')  # time at next scr refresh
            genuine_img.setAutoDraw(True)
        # *genuine_ms* updates
        if genuine_ms.status == NOT_STARTED and genuine_slider.rating:
            # keep track of start time/frame for later
            genuine_ms.frameNStart = frameN  # exact frame index
            genuine_ms.tStart = t  # local t and not account for scr refresh
            genuine_ms.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(genuine_ms, 'tStartRefresh')  # time at next scr refresh
            genuine_ms.status = STARTED
            genuine_ms.mouseClock.reset()
            prevButtonState = genuine_ms.getPressed()  # if button is down already this ISN'T a new click
        if genuine_ms.status == STARTED:  # only update if started and not finished!
            buttons = genuine_ms.getPressed()
            if buttons != prevButtonState:  # button state changed?
                prevButtonState = buttons
                if sum(buttons) > 0:  # state changed to a new click
                    # check if the mouse was inside our 'clickable' objects
                    gotValidClick = False
                    try:
                        iter(genuine_img)
                        clickableList = genuine_img
                    except:
                        clickableList = [genuine_img]
                    for obj in clickableList:
                        if obj.contains(genuine_ms):
                            gotValidClick = True
                            genuine_ms.clicked_name.append(obj.name)
                    if gotValidClick:  
                        continueRoutine = False  # abort routine on response
        
        # *genuine_txt2* updates
        if genuine_txt2.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            genuine_txt2.frameNStart = frameN  # exact frame index
            genuine_txt2.tStart = t  # local t and not account for scr refresh
            genuine_txt2.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(genuine_txt2, 'tStartRefresh')  # time at next scr refresh
            genuine_txt2.setAutoDraw(True)
        
        # check for quit (typically the Esc key)
        if endExpNow or defaultKeyboard.getKeys(keyList=["escape"]):
            core.quit()
        
        # check if all components have finished
        if not continueRoutine:  # a component has requested a forced-end of Routine
            break
        continueRoutine = False  # will revert to True if at least one component still running
        for thisComponent in GENUINEComponents:
            if hasattr(thisComponent, "status") and thisComponent.status != FINISHED:
                continueRoutine = True
                break  # at least one component has not yet finished
        
        # refresh the screen
        if continueRoutine:  # don't flip if this routine is over or we'll get a blank screen
            win.flip()
    
    # -------Ending Routine "GENUINE"-------
    for thisComponent in GENUINEComponents:
        if hasattr(thisComponent, "setAutoDraw"):
            thisComponent.setAutoDraw(False)
    Trials_loop.addData('genuine_slider.response', genuine_slider.getRating())
    Trials_loop.addData('genuine_slider.rt', genuine_slider.getRT())
    Trials_loop.addData('genuine_slider.started', genuine_slider.tStartRefresh)
    Trials_loop.addData('genuine_slider.stopped', genuine_slider.tStopRefresh)
    # store data for Trials_loop (TrialHandler)
    Trials_loop.addData('genuine_ms.started', genuine_ms.tStart)
    Trials_loop.addData('genuine_ms.stopped', genuine_ms.tStop)
    # the Routine "GENUINE" was not non-slip safe, so reset the non-slip timer
    routineTimer.reset()
    
    # ------Prepare to start Routine "EMOTION"-------
    continueRoutine = True
    # update component parameters for each repeat
    np.random.shuffle(button_pos)
    fear.setPos((button_pos[0], 0))
    anger.setPos((button_pos[1], 0))
    happiness.setPos((button_pos[2], 0))
    # setup some python lists for storing info about the emotion_ms
    emotion_ms.x = []
    emotion_ms.y = []
    emotion_ms.leftButton = []
    emotion_ms.midButton = []
    emotion_ms.rightButton = []
    emotion_ms.time = []
    emotion_ms.clicked_name = []
    gotValidClick = False  # until a click is received
    # keep track of which components have finished
    EMOTIONComponents = [emotion_txt, fear, anger, happiness, emotion_ms]
    for thisComponent in EMOTIONComponents:
        thisComponent.tStart = None
        thisComponent.tStop = None
        thisComponent.tStartRefresh = None
        thisComponent.tStopRefresh = None
        if hasattr(thisComponent, 'status'):
            thisComponent.status = NOT_STARTED
    # reset timers
    t = 0
    _timeToFirstFrame = win.getFutureFlipTime(clock="now")
    EMOTIONClock.reset(-_timeToFirstFrame)  # t0 is time of first possible flip
    frameN = -1
    
    # -------Run Routine "EMOTION"-------
    while continueRoutine:
        # get current time
        t = EMOTIONClock.getTime()
        tThisFlip = win.getFutureFlipTime(clock=EMOTIONClock)
        tThisFlipGlobal = win.getFutureFlipTime(clock=None)
        frameN = frameN + 1  # number of completed frames (so 0 is the first frame)
        # update/draw components on each frame
        
        # *emotion_txt* updates
        if emotion_txt.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            emotion_txt.frameNStart = frameN  # exact frame index
            emotion_txt.tStart = t  # local t and not account for scr refresh
            emotion_txt.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(emotion_txt, 'tStartRefresh')  # time at next scr refresh
            emotion_txt.setAutoDraw(True)
        
        # *fear* updates
        if fear.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            fear.frameNStart = frameN  # exact frame index
            fear.tStart = t  # local t and not account for scr refresh
            fear.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(fear, 'tStartRefresh')  # time at next scr refresh
            fear.setAutoDraw(True)
        
        # *anger* updates
        if anger.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            anger.frameNStart = frameN  # exact frame index
            anger.tStart = t  # local t and not account for scr refresh
            anger.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(anger, 'tStartRefresh')  # time at next scr refresh
            anger.setAutoDraw(True)
        
        # *happiness* updates
        if happiness.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            happiness.frameNStart = frameN  # exact frame index
            happiness.tStart = t  # local t and not account for scr refresh
            happiness.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(happiness, 'tStartRefresh')  # time at next scr refresh
            happiness.setAutoDraw(True)
        # *emotion_ms* updates
        if emotion_ms.status == NOT_STARTED and t >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            emotion_ms.frameNStart = frameN  # exact frame index
            emotion_ms.tStart = t  # local t and not account for scr refresh
            emotion_ms.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(emotion_ms, 'tStartRefresh')  # time at next scr refresh
            emotion_ms.status = STARTED
            emotion_ms.mouseClock.reset()
            prevButtonState = emotion_ms.getPressed()  # if button is down already this ISN'T a new click
        if emotion_ms.status == STARTED:  # only update if started and not finished!
            buttons = emotion_ms.getPressed()
            if buttons != prevButtonState:  # button state changed?
                prevButtonState = buttons
                if sum(buttons) > 0:  # state changed to a new click
                    # check if the mouse was inside our 'clickable' objects
                    gotValidClick = False
                    try:
                        iter([happiness,fear,anger])
                        clickableList = [happiness,fear,anger]
                    except:
                        clickableList = [[happiness,fear,anger]]
                    for obj in clickableList:
                        if obj.contains(emotion_ms):
                            gotValidClick = True
                            emotion_ms.clicked_name.append(obj.name)
                    if gotValidClick:
                        x, y = emotion_ms.getPos()
                        emotion_ms.x.append(x)
                        emotion_ms.y.append(y)
                        buttons = emotion_ms.getPressed()
                        emotion_ms.leftButton.append(buttons[0])
                        emotion_ms.midButton.append(buttons[1])
                        emotion_ms.rightButton.append(buttons[2])
                        emotion_ms.time.append(emotion_ms.mouseClock.getTime())
                    if gotValidClick:
                        continueRoutine = False  # abort routine on response
        
        # check for quit (typically the Esc key)
        if endExpNow or defaultKeyboard.getKeys(keyList=["escape"]):
            core.quit()
        
        # check if all components have finished
        if not continueRoutine:  # a component has requested a forced-end of Routine
            break
        continueRoutine = False  # will revert to True if at least one component still running
        for thisComponent in EMOTIONComponents:
            if hasattr(thisComponent, "status") and thisComponent.status != FINISHED:
                continueRoutine = True
                break  # at least one component has not yet finished
        
        # refresh the screen
        if continueRoutine:  # don't flip if this routine is over or we'll get a blank screen
            win.flip()
    
    # -------Ending Routine "EMOTION"-------
    for thisComponent in EMOTIONComponents:
        if hasattr(thisComponent, "setAutoDraw"):
            thisComponent.setAutoDraw(False)
    if emotion_ms.clicked_name[-1] == emotion:
        thisExp.addData("accuracy", 1)
    else:
        thisExp.addData("accuracy", 0)
    # store data for Trials_loop (TrialHandler)
    Trials_loop.addData('emotion_ms.x', emotion_ms.x)
    Trials_loop.addData('emotion_ms.y', emotion_ms.y)
    Trials_loop.addData('emotion_ms.leftButton', emotion_ms.leftButton)
    Trials_loop.addData('emotion_ms.midButton', emotion_ms.midButton)
    Trials_loop.addData('emotion_ms.rightButton', emotion_ms.rightButton)
    Trials_loop.addData('emotion_ms.time', emotion_ms.time)
    Trials_loop.addData('emotion_ms.clicked_name', emotion_ms.clicked_name)
    Trials_loop.addData('emotion_ms.started', emotion_ms.tStart)
    Trials_loop.addData('emotion_ms.stopped', emotion_ms.tStop)
    # the Routine "EMOTION" was not non-slip safe, so reset the non-slip timer
    routineTimer.reset()
    
    # ------Prepare to start Routine "INTENSITY"-------
    continueRoutine = True
    # update component parameters for each repeat
    intensity_slider.reset()
    # setup some python lists for storing info about the intensity_ms
    intensity_ms.clicked_name = []
    gotValidClick = False  # until a click is received
    # keep track of which components have finished
    INTENSITYComponents = [intensity_txt, intensity_slider, intensity_img, intensity_ms, intensity_txt2]
    for thisComponent in INTENSITYComponents:
        thisComponent.tStart = None
        thisComponent.tStop = None
        thisComponent.tStartRefresh = None
        thisComponent.tStopRefresh = None
        if hasattr(thisComponent, 'status'):
            thisComponent.status = NOT_STARTED
    # reset timers
    t = 0
    _timeToFirstFrame = win.getFutureFlipTime(clock="now")
    INTENSITYClock.reset(-_timeToFirstFrame)  # t0 is time of first possible flip
    frameN = -1
    
    # -------Run Routine "INTENSITY"-------
    while continueRoutine:
        # get current time
        t = INTENSITYClock.getTime()
        tThisFlip = win.getFutureFlipTime(clock=INTENSITYClock)
        tThisFlipGlobal = win.getFutureFlipTime(clock=None)
        frameN = frameN + 1  # number of completed frames (so 0 is the first frame)
        # update/draw components on each frame
        
        # *intensity_txt* updates
        if intensity_txt.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            intensity_txt.frameNStart = frameN  # exact frame index
            intensity_txt.tStart = t  # local t and not account for scr refresh
            intensity_txt.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(intensity_txt, 'tStartRefresh')  # time at next scr refresh
            intensity_txt.setAutoDraw(True)
        
        # *intensity_slider* updates
        if intensity_slider.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            intensity_slider.frameNStart = frameN  # exact frame index
            intensity_slider.tStart = t  # local t and not account for scr refresh
            intensity_slider.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(intensity_slider, 'tStartRefresh')  # time at next scr refresh
            intensity_slider.setAutoDraw(True)
        
        # *intensity_img* updates
        if intensity_img.status == NOT_STARTED and intensity_slider.rating:
            # keep track of start time/frame for later
            intensity_img.frameNStart = frameN  # exact frame index
            intensity_img.tStart = t  # local t and not account for scr refresh
            intensity_img.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(intensity_img, 'tStartRefresh')  # time at next scr refresh
            intensity_img.setAutoDraw(True)
        # *intensity_ms* updates
        if intensity_ms.status == NOT_STARTED and intensity_slider.rating:
            # keep track of start time/frame for later
            intensity_ms.frameNStart = frameN  # exact frame index
            intensity_ms.tStart = t  # local t and not account for scr refresh
            intensity_ms.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(intensity_ms, 'tStartRefresh')  # time at next scr refresh
            intensity_ms.status = STARTED
            intensity_ms.mouseClock.reset()
            prevButtonState = intensity_ms.getPressed()  # if button is down already this ISN'T a new click
        if intensity_ms.status == STARTED:  # only update if started and not finished!
            buttons = intensity_ms.getPressed()
            if buttons != prevButtonState:  # button state changed?
                prevButtonState = buttons
                if sum(buttons) > 0:  # state changed to a new click
                    # check if the mouse was inside our 'clickable' objects
                    gotValidClick = False
                    try:
                        iter(intensity_img)
                        clickableList = intensity_img
                    except:
                        clickableList = [intensity_img]
                    for obj in clickableList:
                        if obj.contains(intensity_ms):
                            gotValidClick = True
                            intensity_ms.clicked_name.append(obj.name)
                    if gotValidClick:  
                        continueRoutine = False  # abort routine on response
        
        # *intensity_txt2* updates
        if intensity_txt2.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            intensity_txt2.frameNStart = frameN  # exact frame index
            intensity_txt2.tStart = t  # local t and not account for scr refresh
            intensity_txt2.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(intensity_txt2, 'tStartRefresh')  # time at next scr refresh
            intensity_txt2.setAutoDraw(True)
        
        # check for quit (typically the Esc key)
        if endExpNow or defaultKeyboard.getKeys(keyList=["escape"]):
            core.quit()
        
        # check if all components have finished
        if not continueRoutine:  # a component has requested a forced-end of Routine
            break
        continueRoutine = False  # will revert to True if at least one component still running
        for thisComponent in INTENSITYComponents:
            if hasattr(thisComponent, "status") and thisComponent.status != FINISHED:
                continueRoutine = True
                break  # at least one component has not yet finished
        
        # refresh the screen
        if continueRoutine:  # don't flip if this routine is over or we'll get a blank screen
            win.flip()
    
    # -------Ending Routine "INTENSITY"-------
    for thisComponent in INTENSITYComponents:
        if hasattr(thisComponent, "setAutoDraw"):
            thisComponent.setAutoDraw(False)
    Trials_loop.addData('intensity_slider.response', intensity_slider.getRating())
    Trials_loop.addData('intensity_slider.rt', intensity_slider.getRT())
    Trials_loop.addData('intensity_slider.started', intensity_slider.tStartRefresh)
    Trials_loop.addData('intensity_slider.stopped', intensity_slider.tStopRefresh)
    # store data for Trials_loop (TrialHandler)
    Trials_loop.addData('intensity_ms.started', intensity_ms.tStart)
    Trials_loop.addData('intensity_ms.stopped', intensity_ms.tStop)
    # the Routine "INTENSITY" was not non-slip safe, so reset the non-slip timer
    routineTimer.reset()
    thisExp.nextEntry()
    
# completed 1.0 repeats of 'Trials_loop'

# get names of stimulus parameters
if Trials_loop.trialList in ([], [None], None):
    params = []
else:
    params = Trials_loop.trialList[0].keys()
# save data for this loop
Trials_loop.saveAsExcel(filename + '.xlsx', sheetName='Trials_loop',
    stimOut=params,
    dataOut=['n','all_mean','all_std', 'all_raw'])

# ------Prepare to start Routine "END"-------
continueRoutine = True
# update component parameters for each repeat
end_kb.keys = []
end_kb.rt = []
_end_kb_allKeys = []
# keep track of which components have finished
ENDComponents = [end_txt, end_kb]
for thisComponent in ENDComponents:
    thisComponent.tStart = None
    thisComponent.tStop = None
    thisComponent.tStartRefresh = None
    thisComponent.tStopRefresh = None
    if hasattr(thisComponent, 'status'):
        thisComponent.status = NOT_STARTED
# reset timers
t = 0
_timeToFirstFrame = win.getFutureFlipTime(clock="now")
ENDClock.reset(-_timeToFirstFrame)  # t0 is time of first possible flip
frameN = -1

# -------Run Routine "END"-------
while continueRoutine:
    # get current time
    t = ENDClock.getTime()
    tThisFlip = win.getFutureFlipTime(clock=ENDClock)
    tThisFlipGlobal = win.getFutureFlipTime(clock=None)
    frameN = frameN + 1  # number of completed frames (so 0 is the first frame)
    # update/draw components on each frame
    
    # *end_txt* updates
    if end_txt.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
        # keep track of start time/frame for later
        end_txt.frameNStart = frameN  # exact frame index
        end_txt.tStart = t  # local t and not account for scr refresh
        end_txt.tStartRefresh = tThisFlipGlobal  # on global time
        win.timeOnFlip(end_txt, 'tStartRefresh')  # time at next scr refresh
        end_txt.setAutoDraw(True)
    
    # *end_kb* updates
    waitOnFlip = False
    if end_kb.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
        # keep track of start time/frame for later
        end_kb.frameNStart = frameN  # exact frame index
        end_kb.tStart = t  # local t and not account for scr refresh
        end_kb.tStartRefresh = tThisFlipGlobal  # on global time
        win.timeOnFlip(end_kb, 'tStartRefresh')  # time at next scr refresh
        end_kb.status = STARTED
        # keyboard checking is just starting
        waitOnFlip = True
        win.callOnFlip(end_kb.clock.reset)  # t=0 on next screen flip
        win.callOnFlip(end_kb.clearEvents, eventType='keyboard')  # clear events on next screen flip
    if end_kb.status == STARTED and not waitOnFlip:
        theseKeys = end_kb.getKeys(keyList=['q'], waitRelease=False)
        _end_kb_allKeys.extend(theseKeys)
        if len(_end_kb_allKeys):
            end_kb.keys = _end_kb_allKeys[-1].name  # just the last key pressed
            end_kb.rt = _end_kb_allKeys[-1].rt
            # a response ends the routine
            continueRoutine = False
    
    # check for quit (typically the Esc key)
    if endExpNow or defaultKeyboard.getKeys(keyList=["escape"]):
        core.quit()
    
    # check if all components have finished
    if not continueRoutine:  # a component has requested a forced-end of Routine
        break
    continueRoutine = False  # will revert to True if at least one component still running
    for thisComponent in ENDComponents:
        if hasattr(thisComponent, "status") and thisComponent.status != FINISHED:
            continueRoutine = True
            break  # at least one component has not yet finished
    
    # refresh the screen
    if continueRoutine:  # don't flip if this routine is over or we'll get a blank screen
        win.flip()

# -------Ending Routine "END"-------
for thisComponent in ENDComponents:
    if hasattr(thisComponent, "setAutoDraw"):
        thisComponent.setAutoDraw(False)
# the Routine "END" was not non-slip safe, so reset the non-slip timer
routineTimer.reset()

# Flip one final time so any remaining win.callOnFlip() 
# and win.timeOnFlip() tasks get executed before quitting
win.flip()

# these shouldn't be strictly necessary (should auto-save)
thisExp.saveAsWideText(filename+'.csv', delim='semicolon')
thisExp.saveAsPickle(filename)
logging.flush()
# make sure everything is closed down
if eyetracker:
    eyetracker.setConnectionState(False)
thisExp.abort()  # or data files will save again on exit
win.close()
core.quit()
