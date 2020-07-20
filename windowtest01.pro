pro windowtest01

; w1 = WINDOW(WINDOW_TITLE="Window 1",DIMENSIONS=[500,300],name='window01')
; p1=plot([2,50,100],[3,60,300],symbol='dot',linestyle=' ',overplot=w1,/device)
; w2 = WINDOW(WINDOW_TITLE="Window 2",DIMENSIONS=[500,300],name='window02')
; p2=plot([2,50,100,300],[3,60,300,100],symbol='+',linestyle=' ',overplot=w1,/device)

imdispsize=[512,512]
win=obj_new('IDLgrWindow',dimension=[512,512],title='Raytrace',retain=2,renderer=1)
view=obj_new('IDLgrView',view=[0,0,imdispsize[0],imdispsize[1]],color=[0,0,0])
grmodel=obj_new('IDLgrModel')
image=obj_new('IDLgrImage',bytscl(dist(512)))
grmodel->add,image

symbol=obj_new('IDLgrSymbol',3,color=[255,0,0])
polyline=obj_new('IDLgrPolyline',findgen(2,10)*50,symbol=symbol)

grmodelpoly=obj_new('IDLgrModel')
grmodelpoly->add,polyline

view->add,grmodel,position=0
view->add,grmodelpoly,position=1

win->draw,view







image->setProperty,data=bytscl(dist(512))

polyline->setProperty,data=findgen(2,10)*50


win->draw,view

polyline->setProperty,data=findgen(2,10)*20,linestyle=6


stop
return
end
