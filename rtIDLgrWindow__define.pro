;  $Id$


FUNCTION rtIDLgrWindow::Init, $
    RENDERER=renderer, _EXTRA=_extra
    if (~self->IDLgrWindow::Init(RENDERER=renderer, $
        _EXTRA=_extra)) then return, 0
    RETURN, 1
END

PRO rtIDLgrWindow::Cleanup
    ; Clean up myself.
    OBJ_DESTROY, self.oContainer
    ; Clean up superclass.
    self->IDLgrWindow::Cleanup
END


PRO rtIDLgrWindow::OnMouseDown, x, y, button, keyMods, nClicks

    ; Only look for left mouse button.
    IF (button NE 1) THEN RETURN

    print,'Mouse down !'


END

PRO rtIDLgrWindow::OnMouseUp, x, y, button

    ; Only look for left mouse button.
    IF (button NE 1) THEN RETURN
    self.buttonDown = 0b

    print,'Mouse up!'


END

PRO rtIDLgrWindow::OnMouseMotion, x, y, keyMods
    print,'Mouse move...'
  

END

PRO rtIDLgrWindow::OnKeyboard, isASCII, Character, keyValue,$
                     x, y, Press, Release, keyMods

    ; Suppress if we have the mouse down and are just
    ; using a modifier key.
    IF (self.buttonDown) THEN RETURN
    ; Ignore ascii characters.
    IF (isASCII) THEN RETURN

    CASE (keyValue) OF
        5: xy = [-10,0]  ; left
        6: xy = [ 10,0]  ; right
        7: xy = [0,10]   ; up
        8: xy = [0,-10]  ; down
        ELSE: RETURN
    ENDCASE

    xy += self.center
    self.pt0 = [0,0,1]
    self.buttonDown = 1b
    self->OnMouseMotion, xy[0], xy[1], 0
    self.buttonDown = 0b
END

PRO rtIDLgrWindow::OnResize, width, height
    ; Recompute the trackball.
    self.center = [width/2.,height/2.]
    self.radius = width
    ; Pass on to superclass to actually change dims.
    self->IDLgrWindow::OnResize, width, height
END

PRO rtIDLgrWindow::OnEnter
    self->NotifyBridge, "rtIDLgrWindow", "OnEnter"
END

PRO rtIDLgrWindow::OnExit
    self->NotifyBridge, "rtIDLgrWindow", "OnExit"
END

PRO rtIDLgrWindow__define
     void = {rtIDLgrWindow, inherits IDLgrWindow, $
        oContainer: OBJ_NEW(), $
        oModel: OBJ_NEW(), $
        center: LONARR(2), $
        radius: 0d, $
        buttonDown: 0b, $
        pt0: DBLARR(3), $
        pt1: DBLARR(3) $
        }
END
