pro testdisp01

wn = obj_new('IDLgrWindow',dimension=[512,512],title='Toto',retain=2,renderer=1)

view = obj_new('IDLgrView',view=[0,0,512,512],color=[0,0,0])
model = obj_new('IDLgrModel')        

imm = bytscl(dist(512))

ima = obj_new('IDLgrImage',imm)

view->add,model,position=0
model->add,ima
wn->draw,view





return
end