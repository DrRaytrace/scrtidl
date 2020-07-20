; $Id$


; ---- compute the cloud
function rtmodel78::calcCloud, ratio, hgt, nbvertazimuth, nbvertaxis

; -- equation of the ellipsoid
; x = a cos(u) cos(v)
; y = a cos(u) sin(v)
; z = c sin(u)

semiaxis = hgt / 2.
radius = semiaxis / ratio

u = lgen(nbvertaxis, -!pi/2., !pi/2.)
v = lgen(nbvertazimuth, -!pi, !pi)

poutershell = fltarr(long(nbvertaxis) * long(nbvertazimuth),3)

for i=0l,nbvertaxis-1 do begin
    poutershell[(i * long(nbvertazimuth)):((i + 1) * long(nbvertazimuth) - 1), *] = $
        [   [radius * cos(u[i]) * cos(v)],$
            [radius * cos(u[i]) * sin(v)],$
            [replicate(semiaxis + semiaxis * sin(u[i]), nbvertazimuth)] ]
endfor


return,transpose(poutershell)
end




; ------ update the cloud
pro rtmodel78::updateCloud

ratio = self->getWidgetValue(self.unamemodelid+'wsli.ratio')
; semiaxis = self->getWidgetValue(self.unamemodelid+'wsli.semiaxis')
hgt = self->getWidgetValue(self.unamemodelid+'wsli.hgt')

nbvertazimuth = float((self->getWidgetValue(self.unamemodelid+'wtex.nbvertazimuth'))[0])
nbvertaxis = float((self->getWidgetValue(self.unamemodelid+'wtex.nbvertaxis'))[0])

oc = self->calcCloud(ratio, hgt, nbvertazimuth, nbvertaxis)

; -- add last point at the surface of the sun, pointing in the direction of the model nose
szoc = size(oc, /dim)
oc = reform([reform(oc,n_elements(oc)),0,0,1.01],3,szoc[1]+1)

self.cloud=ptr_new(oc)

return
end

; ------ update neang
pro rtmodel78::updateNeang
lon=self->getLonSliderinCarrCoord()
lat=self->getWidgetValue(self.unamemodelid+'wsli.lat')
rot=self->getWidgetValue(self.unamemodelid+'wsli.rot')

self.neang=[lon,lat,rot]*!dtor
return
end


; ------ update nepos (overload)
pro rtmodel78::updateNepos

self->rtmodelbase::updateNepos

hgt = self->getWidgetValue(self.unamemodelid+'wsli.hgt')

self.nerotcntr = [0.,0,hgt]

return
end


function rtmodel78::getModParam

radius = self->getWidgetValue(self.unamemodelid+'wsli.ratio')
; semiaxis = self->getWidgetValue(self.unamemodelid+'wsli.semiaxis')
hgt = self->getWidgetValue(self.unamemodelid+'wsli.hgt')

mp=[ratio, hgt]

self.pmodparam=ptr_new(mp)

return,self.pmodparam
end



; ------ event handler
function rtmodel78::eventHandler,ev,uname
if strmid(uname,0,4) ne self.unamemodelid then return,0b

foo=self->rtmodelbase::eventHandler(ev,uname)
if foo ne 0 then return,foo

widgettype=strmid(uname,4,4)
flagupdatecloud=0b
flagupdateneang=0b
; --flagout :  b1: refresh only , b0 : recalcViews
flagout='00'b
case widgettype of
    'wsli' : begin
        slidername=strmid(uname,9)
;         print,'slidername : ',slidername
        flagupdatecloud=(slidername eq 'ratio' || slidername eq 'hgt' || slidername eq 'rma')
        flagupdateneang=1b
        flagout='01'b
    end
    'wtex' : begin
        flagupdatecloud=1b
        flagupdateneang=0b
        flagout='01'b
    end
    'wbut' : begin
        slidername=strmid(uname,9)
        ; -- wire on/off button
        if slidername eq 'wireonoff' then begin
            ; -- get value
            widget_control,ev.id,get_value=val
            ; -- switch value
            self.wireonoff=val eq 'Wire On'
            widget_control,ev.id,set_value=(self.wireonoff ? 'Wire Off' : 'Wire On')
            flagout='11'b
        endif

    end
endcase

; ---- recompute cloud in case of
if flagupdatecloud then self->updateCloud

; ---- recompute neang
if flagupdateneang then begin
    self->updateNeang
    self->updateNepos
endif
return,flagout
end


; ------ build the model widget
function rtmodel78::buildWidget,wparent

self.wbase = widget_base(wparent,/column,$
    title=strtrim(self.instanceid,2)+':'+self.modelname,uname=self.unamemodelid+'base')
wtab = widget_tab(self.wbase,/align_left)

; ---- show current wireframe color of the model
self->rtmodelbase::buildCloudColorBox


; ---- build sliders common to all models
wtab_position = widget_base(wtab,/column,title='Position')
self->rtmodelbase::buildPositionGUI,wtab_position,self.unamemodelid

; ---- build model specific sliders
; -- Height slider
self->rtmodelbase::buildHeightGUI,wtab_position,self.unamemodelid,$
    heightinit = 10.

; -- semi-axis ratio
wslider_ratio = cw_fslider(wtab_position, minimum=0.001,maximum=10.,$
    value=1.,$ 
    title='Aspect Ratio',uname=self.unamemodelid+'wsli.ratio',/drag,/edit,xsize=360)
self.guiunamelist.add,'wsli.ratio'

; -- semi axis length
; wslider_semiaxis = cw_fslider(wtab_position,minimum=0.001,maximum=10.,$
;     value=1.,$ 
;     title='Semi-axis length',uname=self.unamemodelid+'wsli.semiaxis',/drag,/edit,xsize=360)
; self.guiunamelist.add,'wsli.semiaxis'


; ---- model specific cloud parameters
wtab_cloud = widget_base(wtab,/column,title='Cloud')

wlabel_nbvertaxis = widget_label(wtab_cloud,value='# Points Axis',$
    uname=self.unamemodelid+'wlab.nbvertaxis')
wtext_nbvertaxis = widget_text(wtab_cloud,/editable,$
    value='50',$
    uname=self.unamemodelid+'wtex.nbvertaxis')
self.guiunamelist.add,'wtex.nbvertaxis'


wlabel_nbvertazimuth = widget_label(wtab_cloud,value='# Points Azimuth',$
    uname=self.unamemodelid+'wlab.nbvertazimuth')
wtext_nbvertazimuth = widget_text(wtab_cloud,/editable,$
    value='50',$
    uname=self.unamemodelid+'wtex.nbvertazimuth')
self.guiunamelist.add, 'wtex.nbvertazimuth'



self->rtmodelbase::buildChangeCloudColor,wtab_cloud,self.unamemodelid

wlabel_msg1 = widget_label(wtab_cloud,$
    value='Press ENTER when done',uname=self.unamemodelid+'wlab.msg1')

wbutton_wireonoff = widget_button(wtab_cloud,value='Wire Off',$
    uname=self.unamemodelid+'wbut.wireonoff')
self.wireonoff=1b



; ---- build common Ne shift sliders
wtab_neshift = widget_base(wtab,/column,title='Ne Shift')
self->rtmodelbase::buildNeShift,wtab_neshift,self.unamemodelid

; ---- build delete button
self->rtmodelbase::buildDeleteButton


self->updateCloud
self->updateNeang

self.allowraytrace=1b

return,self.wbase
end

; ------ constructor
function rtmodel78::INIT,instanceid,_extra=extra

foo=self->rtmodelbase::init('Ellipsoid', 78, instanceid, _extra=extra)

return,1
end


; ------ class structure definition
pro rtmodel78__define
    struct={rtmodel78, inherits rtmodelbase}
return
end

; $Log$
;

