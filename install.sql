--***************************
-- PLDOCU INSTALLATION SCRIPT
--***************************
set scan off;
prompt => Start installation process
@pldocu.pks
@pldocu.pkb
prompt => Set grant and synonym to public
create public synonym pldocu for pldocu;
grant execute on pldocu to public;
exit
