create or replace package body pldocu
as
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
c_package_type constant user_objects.object_type%type:='PACKAGE';
eol varchar2(2 char);
sep varchar2(1 char);
-- argument parse info
type t_arg_pinfo is record(
  dsc varchar2(1000 char),  -- argument description text
  exv varchar2(1000 char)); -- example value
type tt_arg_pinfos is table of t_arg_pinfo index by pls_integer;
-- subprogram parse infos
type t_sub_pinfo is record(
  pkg varchar2(1000 char),  -- package description text
  par varchar2(255 char),   -- package author
  nam varchar2(30 char),    -- name
  typ varchar2(30 char),    -- "type" (procedure, function or (sub)type)
  dsc varchar2(1000 char),  -- description text
  sar varchar2(255 char),   -- subprogram author
  arg tt_arg_pinfos,        -- description of arguments
  exd varchar2(1000 char)); -- example description
type tt_sub_pinfos is table of t_sub_pinfo index by pls_integer;
-- enclosing chars for different markups and different keywords
type t_vc2_aa is table of varchar2(1000 char) index by varchar2(30 char);
type tt_vc2_aa is table of t_vc2_aa index by varchar2(30 char);
l_t9n tt_vc2_aa;
l_pfx tt_vc2_aa;
l_sfx tt_vc2_aa;
-- cursor for parse infos
cursor cur_user_source(p_package_name in varchar2) 
is
  with constants as (
    select '((FUNCTION|PROCEDURE)\s+)' as fp,
           '((TYPE|SUBTYPE)\s+)' as ts,
           '([A-Z0-9_\$\#])' as vc,
           '(FUNCTION|PROCEDURE|TYPE)\s+' as fpt
      from dual
  ), sourcecode as (
  select us.line as line,
         upper(trim(text)) as utt,
         trim(text) as tt
    from user_source us
   where type=c_package_type
     and name=p_package_name
  )
  select 
    regexp_count(s.utt,'(^AS|AS$)|(^IS|IS$)')             as pkg_end,
    regexp_count(s.utt,'^'||w.ts||'('||w.vc||'*)')        as type_beg,
    regexp_count(s.utt, '^'||w.fp||'('||w.vc||'*)')       as subprogram_sig_beg,
    sum( regexp_count(s.utt, '^'||w.fp||'('||w.vc||'*)') ) 
      over (order by s.line rows unbounded preceding)     as subprogram_id,
    case when regexp_substr(s.tt,'(^\s*--\s*@+)(.*)',1,1,'x',2) is null
      then 0
      else 1
    end                                                   as arg_comment_beg,
    regexp_count(s.utt,'(.*)((;){1})')                    as statement_end,
    regexp_substr(s.utt,
      '^\s*'||w.fpt||'('||w.vc||'*)',1,1,'x',2)           as object_name,
    regexp_substr(s.utt,
      '^\s*'||w.fpt||'('||w.vc||'*)',1,1,'x',1)           as object_type,
    rtrim(s.tt,eol)                                       as trimmed_text,
    trim(regexp_substr(s.tt,'(^\s*--\s*)(.*)',1,1,'x',2)) as trimmed_comment,
    regexp_substr(s.tt,'{(.*)}\s$',1,1,'ix',1)            as author_name,
    regexp_substr(s.tt,'\[(.*)\]\s+$',1,1,'ix',1)         as example_value
  from sourcecode s
  cross join constants w
  order by line; 
-- cursor for docu lookup infos
cursor cur_docu_lookup 
is
  with fmt as (
    select column_value as fmt 
      from table(sys.dbms_debug_vc2coll('MD','HTML','RST'))
  ), code as (
    select column_value as code 
      from table(sys.dbms_debug_vc2coll('META',
                                        'H1','H2','H3','P','PRE',
                                        'THEAD','THSEP','TCELL','TCSEP','TFOOT'))
  ), newline as (
    select eol as eol from dual
  )
  select
    fmt,
    code,
    case 
      when fmt='HTML' then
        case code when 'THEAD'  then '<table><thead><tr><th>'
                  when 'THSEP'  then '</th><th>'
                  when 'TCELL'  then '<tr><td>'
                  when 'TCSEP'  then '</td><td>'
                  when 'TFOOT'  then null
                  when 'META'   then null
                                else '<'||lower(code)||'>'
        end
      when fmt='MD' then
        case code when 'H1'     then '# '
                  when 'H2'     then '## '
                  when 'H3'     then '### '
                  when 'P'      then ''
                  when 'PRE'    then '```PLSQL'||eol
                  when 'THEAD'  then '| '
                  when 'THSEP'  then ' | '
                  when 'TCELL'  then '| '
                  when 'TCSEP'  then ' | '
        end
      when fmt='RST' then
        case code when 'META'   then '===================='||eol||
                                     'PLDOCU documentation'||eol||
                                     '===================='||eol||eol||
                                     '------------------------'||eol||
                                     'Created from PLSQL code.'||eol||
                                     '------------------------'||eol||eol
                  when 'PRE'    then '.. code-block:: SQL'||eol||eol
                  when 'THEAD'  then '='||eol
                  when 'THSEP'  then ' '
                  when 'TCSEP'  then ' '
        end
    end as pfx,
    case
      when fmt='HTML' then 
        case code when 'THEAD'  then '</th></tr></thead><tbody>'
                  when 'TCELL'  then '</td></tr>'
                  when 'TFOOT'  then '</tbody></table>'
                  when 'META'   then null
                                else '</'||lower(code)||'>'||eol
        end
      when fmt='MD' then
        case code when 'P' then eol
                  when 'PRE' then eol||'```'||eol
                  when 'THEAD'  then ' |'||eol||'|---|---|'
                  when 'TCELL'  then ' |'
        end
      when fmt='RST' then
        case code when 'H1'     then eol||'='||eol
                  when 'H2'     then eol||'-'||eol
                  when 'H3'     then eol||'`'||eol
                  when 'P'      then eol
                  when 'PRE'    then eol
                  when 'THEAD'  then eol||'='||eol 
                  when 'TFOOT'  then eol||'='
        end
    end||eol as sfx
  from fmt, code, newline;
-- cursor for translation of keywords
cursor cur_translation
is
  with lang as (
  select column_value as lng
    from table(sys.dbms_debug_vc2coll('en','de','es'))
  ), code as (
  select column_value as cd
    from table(sys.dbms_debug_vc2coll('Parameter','Parameters','Description'))
  )  
  select 
    lng,
    cd,
    case lng 
      when 'de' then decode(cd,'Parameters','Parameter',
                               'Parameter','Parametername',
                               'Description','Parameterbeschreibung',
                            cd)
      when 'es' then decode(cd,'Description','Descripción',
                            cd)
      else cd
    end as val
  from lang, code;
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
function parse_package(a_pkg_name in varchar2)
  return tt_sub_pinfos
is
  l_package_name user_objects.object_name%type not null:=upper(a_pkg_name);
  l_sub_pinfo t_sub_pinfo;
  l_sub_pinfos tt_sub_pinfos;
  l_arg_pinfos tt_arg_pinfos;
  l_sub_key simple_integer:=0;
  l_arg_key simple_integer:=0;
  l_is_pkg_parse boolean:=true;  -- handle package parse mode
  l_is_sub_parse boolean:=false; -- handle subprogram parse mode
  l_is_arg_parse boolean:=false; -- handle arg parse mode
  procedure set_back_to_initial_values is
  begin
    l_sub_pinfo:=null;
    l_arg_pinfos.delete;
    l_arg_key:=0;
    l_is_sub_parse:=false;
    l_is_arg_parse:=false;
  end set_back_to_initial_values;
begin
  <<parse_source_code>>
  for i in cur_user_source(l_package_name)
  loop
    -- beginning = package description
    if l_is_pkg_parse and i.pkg_end=0 then
      -- n package description text
      if i.trimmed_comment is not null then
        l_sub_pinfo.pkg:=l_sub_pinfo.pkg||i.trimmed_comment||' ';
        l_sub_pinfo.par:=i.author_name;
      end if;
      continue;
    else
      l_is_pkg_parse:=false;
    end if;
    -- 1 object name
    if i.object_name is not null then
      l_sub_pinfo.nam:=i.object_name;
    end if;
    -- 1 object type
    if i.object_type is not null then
      l_sub_pinfo.typ:=i.object_type;
    end if;
    -- key reading
    if i.subprogram_sig_beg=1 then
      l_is_sub_parse:=true;
      l_sub_key:=l_sub_key+1;
    end if;
    -- n argument description text
    if (i.arg_comment_beg=1 or l_is_arg_parse) and not l_is_sub_parse then   
      l_is_arg_parse:=true;
      if substr(i.trimmed_comment,1,1)='@' then
        l_arg_key:=l_arg_key+1;
        l_arg_pinfos(l_arg_key).dsc:=ltrim(i.trimmed_comment,'@');
      else
        l_arg_pinfos(l_arg_key).dsc:=
          l_arg_pinfos(l_arg_key).dsc||' '||i.trimmed_comment;
      end if;
      l_arg_pinfos(l_arg_key).exv:=i.example_value;
      l_arg_pinfos(l_arg_key).dsc:=
        trim(regexp_substr(l_arg_pinfos(l_arg_key).dsc,'[^\[]*'));
    end if;
    -- n object description text
    if i.trimmed_comment is not null and not l_is_arg_parse then
      l_sub_pinfo.dsc:=l_sub_pinfo.dsc||i.trimmed_comment||' ';
      l_sub_pinfo.sar:=i.author_name;
    end if;
    -- stop signature (key) reading and save collected infos
    if l_is_sub_parse and i.statement_end=1 then
      -- remove author info
      l_sub_pinfo.pkg:=trim(regexp_substr(l_sub_pinfo.pkg,'[^{]*'));
      l_sub_pinfo.dsc:=trim(regexp_substr(l_sub_pinfo.dsc,'[^{]*'));
      -- assign arg collection to sub record, then sub record to sub collection
      l_sub_pinfo.arg:=l_arg_pinfos;
      l_sub_pinfos(l_sub_key):=l_sub_pinfo;
      set_back_to_initial_values;
    end if;
  end loop parse_source_code;
  return l_sub_pinfos;
end parse_package;
--------------------------------------------------------------------------------
function package_infos(a_pkg_name in varchar2)
  return tt_pkg_infos pipelined deterministic
is
  l_package_name user_objects.object_name%type not null:=upper(a_pkg_name);
  l_sub_pinfos tt_sub_pinfos:=parse_package(l_package_name);
begin
  for r in (
  select 
    object_id           as pkg_id,
    object_name         as pkg_name,
    l_sub_pinfos(1).pkg  as pkg_desc,
    l_sub_pinfos(1).par  as pkg_author,
    (
      select max("AUTHID") 
        from user_procedures 
       where object_name=l_package_name
    )                   as pkg_rights,
    created             as create_time,
    last_ddl_time       as update_time
  from user_objects
  where object_name=l_package_name
  and object_type=c_package_type
  )
  loop
    pipe row(r);
  end loop;
  return;
end package_infos;
--------------------------------------------------------------------------------
function subprogram_infos(a_pkg_name in varchar2)
  return tt_sub_infos pipelined deterministic
is
  l_package_name user_objects.object_name%type not null:=upper(a_pkg_name);
  l_sub_pinfos tt_sub_pinfos:=parse_package(l_package_name);
begin
  for r in (
  select 
    object_id                 as pkg_id,
    subprogram_id             as sub_id,
    procedure_name            as sub_name,
    to_char(null)             as sub_desc,
    to_char(null)             as sub_author,
    to_char(null)             as sub_type,
    length("DETERMINISTIC")-2 as is_deterministic,
    length("PIPELINED")-2     as is_pipelined
  from user_procedures
  where object_name=l_package_name
  and procedure_name is not null
  order by subprogram_id, overload
  ) 
  loop
    r.sub_desc:=l_sub_pinfos(r.sub_id).dsc;
    r.sub_author:=l_sub_pinfos(r.sub_id).sar;
    r.sub_type:=l_sub_pinfos(r.sub_id).typ;
    pipe row(r);
  end loop;
  return;
end subprogram_infos;
--------------------------------------------------------------------------------
function argument_infos(a_pkg_name in varchar2 )
  return tt_arg_infos pipelined deterministic
is
  l_package_name user_objects.object_name%type not null:=upper(a_pkg_name);
  l_sub_pinfos tt_sub_pinfos:=parse_package(l_package_name);
begin
  for r in (
  select 
    object_id                 as pkg_id,
    subprogram_id             as sub_id,
    to_number(lpad(subprogram_id,2,'0')
    || lpad(sequence,2,'0') ) as arg_id,
    argument_name             as arg_name,
    null                      as arg_desc,
    null                      as arg_exam,
    sequence                  as arg_seq,
    position                  as arg_pos,
    pls_type                  as pls_type,
    data_type                 as sql_type,
    to_number(null)           as is_default,
    instr(in_out,'IN')        as is_in,
    sign(instr(in_out,'OUT')) as is_out,
    null                      as default_value
  from user_arguments
  where package_name=l_package_name
  and data_level=0                                                              -- ???  0=direct, 1=record/pipelined, 2=columns in record
  order by subprogram_id, overload, decode(position,0,999)
  )
  loop
    if l_sub_pinfos(r.sub_id).arg.count>0 and r.arg_pos>0 then
      r.arg_desc:=l_sub_pinfos(r.sub_id).arg(r.arg_pos).dsc;
      r.arg_exam:=l_sub_pinfos(r.sub_id).arg(r.arg_pos).exv;      
    end if;
    pipe row(r);
  end loop;
  return;
end argument_infos;
--------------------------------------------------------------------------------
function subprogram_argument_infos(a_pkg_name in varchar2)
  return tt_sa_infos pipelined deterministic
is
  l_package_name user_objects.object_name%type not null:=upper(a_pkg_name);
begin
  for r in (
  with syntax_info as (
    select object_id      as pkg_id,
           subprogram_id  as sub_id,
           position       as arg_pos,
           argument_name  as arg_name,
           case when position>0                                                 -- 0=return from function 
            then '  '
            ||rpad(lower(argument_name), 
                   max(length(argument_name)) over (partition by subprogram_id)+2,
                   ' ')
            ||rpad(in_out, max(case when position>0 then length(in_out) end) over (partition by subprogram_id),' ')
            ||'  '
            ||case when data_type in ('TABLE','PL/SQL TABLE')
                then case when type_owner<>user 
                      then type_owner||'.' 
                     end||type_name||'.'||type_subname 
                else data_type 
              end
           end as args_fmt,
           case when position=0 
            then case when data_type in ('TABLE','PL/SQL TABLE')
                        then case when type_owner<>user 
                              then type_owner||'.' 
                             end||type_name||'.'||type_subname 
                        else data_type 
                 end
           end as return_type,           
           case when position=0 and data_type='TABLE' 
            then 1
           end as is_pipelined,
           ora_hash(pls_type) as pls_type_hash,
           ora_hash(data_type) as sql_type_hash
      from user_arguments
     where package_name=l_package_name
       and data_level=0                                                         -- 1=Records, 2=DataTypes in Record
  ),
  argument_info as (
    select pkg_id,
           sub_id,
           arg_pos,
           arg_name||sep||arg_desc as args,
           arg_exam
      from table(pldocu.argument_infos(l_package_name))
     where arg_pos>0
  )
    select pkg_id, 
           sub_id,
           case when count(*)>1 or (max(return_type) is null and count(*)=1) 
            then ' (' 
           end
           ||eol
           ||listagg(args_fmt,','||eol) within group (order by arg_pos)
           ||case when max(return_type) is null 
              then ')' 
              else 
                case when count(arg_name)>0 then ')' end
                ||eol||'RETURN '||max(return_type)
            end ||';'
           as syntax,
           listagg(args, eol) within group (order by arg_pos) as arguments,
           -- example?
           case 
            when max(arg_exam) is null 
              then null
            -- procedure?
            when max(return_type) is null
              then 'BEGIN >procedure< END;'
            -- function pipelined?
            when sum(is_pipelined)=1
              then 'SELECT *'||eol||'  FROM TABLE('||lower(max(return_type))||') >pipelined function<;'
            -- SQL function?
            when sum(sql_type_hash)=sum(pls_type_hash)
              then 'SELECT >sql function< FROM dual;'
            -- PLSQL function
              else 'DECLARE BEGIN >pslql function< END;'              
           end examples
      from syntax_info si
 left join argument_info ai using (pkg_id, sub_id, arg_pos)
  group by pkg_id, sub_id
  order by sub_id
  )
  loop
    pipe row(r);
  end loop;
end subprogram_argument_infos;
--------------------------------------------------------------------------------
procedure parse_debug(
    a_pkg_name in varchar2 )
is
  l_key pls_integer;
  l_package_name user_objects.object_name%type not null:=upper(a_pkg_name);
  l_spi tt_sub_pinfos:=parse_package(l_package_name);
  procedure print_if_not_null(a_check in varchar2, a_pfx in varchar2) is
  begin
    if a_check is not null then
      dbms_output.put_line(a_pfx||a_check);    
    end if;
  end;
begin
  l_key:=l_spi.first;
  print_if_not_null(l_spi(l_key).pkg, 'PKG=');
  print_if_not_null(l_spi(l_key).par, 'PAR=');
  while (l_key is not null)
  loop
    dbms_output.new_line;
    dbms_output.put_line('Key='||l_key);
    dbms_output.put_line('  Nam='||l_spi(l_key).nam);
    dbms_output.put_line('  Typ='||l_spi(l_key).typ);
    print_if_not_null(l_spi(l_key).dsc, '  Dsc=');
    print_if_not_null(l_spi(l_key).sar, '  Ath=');
    -- to avoid error ORA-06531: Reference to uninitialized collection
    if l_spi(l_key).arg is not null then
      for i in 1..l_spi(l_key).arg.count
      loop
        print_if_not_null(l_spi(l_key).arg(i).dsc, '    Arg'||i||'.Dsc=');
        print_if_not_null(l_spi(l_key).arg(i).exv, '    Arg'||i||'.Exv=');
      end loop;
    end if;
    l_key:=l_spi.next(l_key);
  end loop;
end parse_debug;
--------------------------------------------------------------------------------
function render_docu(
    a_pkg_name in varchar2,
    a_fmt_name in varchar2)
  return varchar2 deterministic
is
  l_pkg user_objects.object_name%type not null:=upper(a_pkg_name);
  l_fmt varchar2(30 char) not null:=upper(a_fmt_name);
  l_out varchar2(4000 char):='';
  l_lang varchar2(2 char);
  l_len pls_integer;
  l_ibeg simple_integer:=0;
  l_iend simple_integer:=0;
  procedure push(a_txt in varchar2, a_code in varchar2 default null) 
  is
    l_prefix varchar2(255 char):=l_pfx(l_fmt)(a_code);
    l_suffix varchar2(255 char):=l_sfx(l_fmt)(a_code);
    l_txt varchar2(4000 char):=a_txt;
  begin
    if l_fmt='RST' then
      if a_code in ('H1','H2','H3') then
        l_prefix:=replace(l_prefix,
                          substr(l_prefix,1,1),
                          lpad(substr(l_prefix,1,1),
                               length(l_txt),
                               substr(l_prefix,1,1))
                         ); 
        l_suffix:=replace(l_suffix,
                          substr(l_suffix,2,1),
                          lpad(substr(l_suffix,2,1),
                               length(l_txt),
                               substr(l_suffix,2,1))
                         );
      elsif a_code in ('THEAD') then
        l_prefix:=lpad('=',l_len,'=')||' '||lpad('=',l_len,'=')||eol;
        l_suffix:=eol||lpad('=',l_len,'=')||' '||lpad('=',l_len,'=')||eol;--||eol;
      elsif a_code ='TFOOT' then
        l_suffix:=lpad('=',l_len,'=')||' '||lpad('=',l_len,'=')||eol||eol;
      elsif a_code='PRE' then
        l_txt:=replace(l_txt,eol,eol||'  ');
        l_txt:='  '||l_txt;
      end if;
    end if;
    
    l_out:=l_out||l_prefix||l_txt||l_suffix;
  end push;
  procedure set_max_col_len(p_snippet in varchar2) is
  begin
    l_len:=0;
    l_ibeg:=0;
    l_iend:=instr(p_snippet, eol);
    if l_iend=0 then
      l_iend:=length(p_snippet)+1; 
    end if;
    
    while(l_iend>0) loop
      l_ibeg:=0;
      l_len:=greatest(l_len,length(
      substr(
        substr(p_snippet, l_ibeg+1, l_iend-l_ibeg-1),
        l_ibeg+1,
        instr(substr(p_snippet, l_ibeg+1, l_iend-l_ibeg-1),sep,l_ibeg+1)
      )
      ));
      l_ibeg:=l_iend;
      l_iend:=instr(p_snippet, eol, l_ibeg+1);  
    end loop;
    if l_ibeg-1<> length(p_snippet) then
      l_ibeg:=0;
      l_len:=greatest(l_len,length(
      substr(
        substr(p_snippet, l_ibeg+1),
        l_ibeg+1,
        instr(substr(p_snippet, l_ibeg+1),sep,l_ibeg+1)
      )
      ));
    end if;
    
    l_len:=greatest(l_len,length(l_t9n(l_lang)('Parameter')));
    l_len:=greatest(l_len,length(l_t9n(l_lang)('Description')));
  end set_max_col_len;
begin
  l_lang:=case userenv('lang')
            when 'D' then 'de'
            when 'E' then 'es'
            else 'en'
          end;
  
  push(null, 'META');
  <<package_infos>>
  for p in (select pkg_name, pkg_desc from table(pldocu.package_infos(l_pkg))) 
  loop
    push(p.pkg_name, 'H1');
    push(p.pkg_desc, 'P');
    
    <<subprogram_infos>>
    for s in (select sub_type||' '||sub_name as sub_header,
                     sub_desc,
                     l_pkg||'.'||sub_name||syntax as sub_syn,
                     arguments as sub_arguments
                from table(pldocu.subprogram_infos(l_pkg))
                join table(pldocu.subprogram_argument_infos(l_pkg)) using (pkg_id, sub_id))
    loop
      push(s.sub_header, 'H2');
      push(s.sub_desc, 'P');           
      push('Syntax', 'H3');
      push(s.sub_syn, 'PRE');
      if s.sub_arguments is not null then
        push(l_t9n(l_lang)('Parameters'), 'H3');
        set_max_col_len(s.sub_arguments);
        -- TABLE HEADER
        push(replace(case when l_fmt='RST'
                      then rpad(l_t9n(l_lang)('Parameter'),l_len,' ')||sep||l_t9n(l_lang)('Description')
                      else l_t9n(l_lang)('Parameter')||sep||l_t9n(l_lang)('Description')
                     end,
                     sep,
                     l_pfx(l_fmt)('THSEP')
                    ), 'THEAD');
        -- TABLE BODY
        l_ibeg:=0;
        l_iend:=instr(s.sub_arguments, eol);
        if l_iend=0 then
          l_iend:=length(s.sub_arguments)+1; 
        end if;
        
        while(l_iend>0) loop
          case when l_fmt='RST' then
          push(
            rpad(
            substr(
              substr(s.sub_arguments, l_ibeg+1, l_iend-l_ibeg-1),
              1,
              instr(substr(s.sub_arguments, l_ibeg+1, l_iend-l_ibeg-1),sep)-1
            )
            ,l_len,' ')||l_pfx(l_fmt)('TCSEP')||
            --||'°'||
            substr(
              substr(s.sub_arguments, l_ibeg+1, l_iend-l_ibeg-1),
              instr(substr(s.sub_arguments, l_ibeg+1, l_iend-l_ibeg-1),sep)+1
            ),
          'TCELL');
          else
          push(replace(substr(s.sub_arguments, l_ibeg+1, l_iend-l_ibeg-1),
                       sep,
                       l_pfx(l_fmt)('TCSEP')
                      ), 'TCELL');
          end case;
          l_ibeg:=l_iend;
          l_iend:=instr(s.sub_arguments, eol, l_ibeg+1);  
        end loop;
        if l_ibeg-1<> length(s.sub_arguments) then
        case when l_fmt='RST' then
          push(
            rpad(
            substr(
              substr(s.sub_arguments, l_ibeg+1),
              1,
              instr(substr(s.sub_arguments, l_ibeg+1),sep)-1
            )
            ,l_len,' ')||l_pfx(l_fmt)('TCSEP')||
            --||'°'||
            substr(
              substr(s.sub_arguments, l_ibeg+1),
              instr(substr(s.sub_arguments, l_ibeg+1),sep)+1
            ),
          'TCELL');
          else
          push(replace(substr(s.sub_arguments, l_ibeg+1),
                       sep,
                       l_pfx(l_fmt)('TCSEP')
                      ), 'TCELL');
          end case;
        end if;
          
        -- TABLE FOOTER
        push(null, 'TFOOT');
      end if;
    
    end loop subprogram_infos;
  end loop package_infos;
  
  return l_out;
  -- CHECK html => https://validator.w3.org/#validate_by_input+with_options
  -- CHECK rst => http://rst.ninjs.org
end render_docu;
--------------------------------------------------------------------------------
procedure compile_code(
    a_pkg_code in clob,
    a_pkg_name out nocopy varchar2)
is
  success_with_compilation_error exception;
  pragma exception_init(success_with_compilation_error, -24344);
  l_pkg_code clob not null:=trim(a_pkg_code);
  l_pkg_name varchar2(30 char);
begin
  l_pkg_name:='SYS_PLDOCU_'||to_char(systimestamp, 'YYMMDD_HH24MISS_FF5');
  -- remove line breaks (if any)
  l_pkg_code:=trim(both eol from l_pkg_code);
  -- remove trailing slash (if any)
  l_pkg_code:=rtrim(l_pkg_code,'/');
  -- remove schema name (if any)
  l_pkg_code:=regexp_replace(l_pkg_code,'PACKAGE\s+"?[A-Z]+"?\.', 'PACKAGE ');
  -- replace old with new package name (header)
  l_pkg_code:=regexp_replace(l_pkg_code,'PACKAGE\s+"?[A-Z]+"?', 
                                        'PACKAGE '||l_pkg_name,
                                        1,1,'i');
  -- replace old with new package name (footer)
  l_pkg_code:=regexp_replace(l_pkg_code,'END\s+"?[A-Z]+"?\s*;', 
                                        'END;',
                                        1,1,'i');
  execute immediate l_pkg_code;
  a_pkg_name:=l_pkg_name;
exception
  when success_with_compilation_error 
    then a_pkg_name:='ERROR';
  when others
    then raise;
end compile_code;
--------------------------------------------------------------------------------
begin
  eol:=case when instr(upper(dbms_utility.port_string),'WIN')>0 
              then chr(13)||chr(10)
            when instr(upper(dbms_utility.port_string),'LIN')>0 
              then chr(10)
            else chr(10)
       end;
  sep:=chr(9);

  <<set_translations>>
  for i in cur_translation
  loop 
    l_t9n(i.lng)(i.cd):=i.val;
  end loop set_translations;
  
  <<docu_lookup>>
  for i in cur_docu_lookup
  loop 
    l_pfx(i.fmt)(i.code):=i.pfx;
    l_sfx(i.fmt)(i.code):=i.sfx;
  end loop docu_lookup;
end pldocu;