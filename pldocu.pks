create or replace package pldocu authid current_user
  -- Package to create documentation from PL/SQL packages. {teotiger}
as
  /****************************************************************************
  *** RECORD TYPES
  ****************************************************************************/
  type t_pkg_info is record(
    pkg_id      user_objects.object_id%type,
    pkg_name    user_objects.object_name%type,
    pkg_desc    varchar2(4000 char),
    pkg_author  varchar2(255 char),
    pkg_rights  varchar2(12 char),
    create_time user_objects.created%type,
    update_time user_objects.last_ddl_time%type);
  type t_sub_info is record(
    pkg_id            user_objects.object_id%type,
    sub_id            user_procedures.subprogram_id%type,
    sub_name          user_procedures.procedure_name%type,
    sub_desc          varchar2(4000 char),
    sub_author        varchar2(255 char),
    sub_type          varchar2(30 char),  -- procedure/function/(sub)type
    is_deterministic  integer,
    is_pipelined      integer);
  type t_arg_info is record(
    pkg_id        user_objects.object_id%type,
    sub_id        user_procedures.subprogram_id%type,
    arg_id        user_arguments.sequence%type,
    arg_name      user_arguments.argument_name%type,
    arg_desc      varchar2(4000 char),
    arg_exam      varchar2(4000 char),
    arg_seq       user_arguments.sequence%type,
    arg_pos       user_arguments.position%type,
    pls_type      user_arguments.pls_type%type,
    sql_type      user_arguments.data_type%type,
    is_default    integer,
    is_in         integer,
    is_out        integer,
    default_value varchar2(4000 char));
  type t_sa_info is record(
    pkg_id        user_objects.object_id%type,
    sub_id        user_procedures.subprogram_id%type,
    syntax        varchar2(4000 char),
    arguments     varchar2(4000 char),
    examples      varchar2(4000 char));
  /****************************************************************************
  *** COLLECTION TYPES
  ****************************************************************************/
  type tt_pkg_infos is table of t_pkg_info;
  type tt_sub_infos is table of t_sub_info;
  type tt_arg_infos is table of t_arg_info;
  type tt_sa_infos is table of t_sa_info;
  /****************************************************************************
  *** SUBPROGRAMS
  ****************************************************************************/
  -- Return information about the package.
  -- @The name of the package.
  function package_infos(
      a_pkg_name in varchar2)
    return tt_pkg_infos pipelined deterministic;
  -- Return information about the subprograms inside the package.
  -- @The name of the package. [APEX_UTIL]
  function subprogram_infos(
      a_pkg_name in varchar2)
    return tt_sub_infos pipelined deterministic;
  -- Return information about all the arguments from subprograms inside the
  -- package.
  -- @The name of the package.
  function argument_infos(
      a_pkg_name in varchar2)
    return tt_arg_infos pipelined deterministic;
  -- Return for each subprogram the syntax specification, a tabular overview of
  -- parameters and a example code snippet.
  -- @The name of the package.
  function subprogram_argument_infos(
      a_pkg_name in varchar2)
    return tt_sa_infos pipelined deterministic;
  -- Write information about the parsing process to DBMS_OUTPUT.
  -- @The name of the package. [PLUTIL]
  procedure parse_debug(
      a_pkg_name in varchar2);
  -- Return all information together in a document format for further usage.
  -- @The name of the package. [PLDOCU]   
  -- @Valid format name like HTML, MD (Markdown) or RST (reStructuredText). [MD]   
  function render_docu(
      a_pkg_name in varchar2,
      a_fmt_name in varchar2)
    return varchar2 deterministic;
  -- Try to compile the package (specification) code with a autogenerated name.
  -- @The source code of the package.
  -- @The generated name of the package for further use. If there is any
  -- problem during compilation, 'ERROR' is returned.
  procedure compile_code(
      a_pkg_code in clob,
      a_pkg_name out nocopy varchar2);
end pldocu;