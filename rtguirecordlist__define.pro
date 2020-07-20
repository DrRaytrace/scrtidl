; ------ add new gui record in the list
pro rtguirecordlist::addRecord,rec
if ptr_valid(self.preclist) then begin
    reclist = *self.preclist
    ptr_free,self.preclist
    self.preclist = ptr_new([reclist,rec])
endif else begin
    self.preclist = ptr_new([rec])
endelse
return
end


function rtguirecordlist::INIT

return,1
end

pro rtguirecordlist::CLEANUP

return
end


pro rtguirecordlist__define
    struct={rtguirecordlist,preclist:ptr_new()}
return
end