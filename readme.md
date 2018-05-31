# PLDocu

## Introduction
PLDocu is a PL/SQL Package to extract the necessary information from your source code to build a documentation.

It provides table functions similarly to the data dictionary views and a function to get a full documentation from a package. The structure follows the documentation from Oracle. The output can be different markup like HTML, Markdown or reST.

## Installation
Simply run the install script from the setup folder inside SQL*Plus or SQL Developer.

## Usage
The following example extract all information from package "PLDOCU":

```
select * 
from table(pldocu.package_infos('PLDOCU'))
natural join table(pldocu.subprogram_infos('PLDOCU'))
natural left join table(pldocu.argument_infos('PLDOCU'))
order by sub_id, arg_id;
```

![Usage example in SQL Developer](usage_example.gif)

## Demo APEX Application

You can try the functionality of the package online: [https://apex.oracle.com/pls/apex/f?p=22122](https://apex.oracle.com/pls/apex/f?p=22122)

## User Defined Extension for Oracle SQL Developer

In SQL Developer go to Tools > Preferences > Database > User Defined Extension.
Click add row and select EDITOR as type. Then select the pldocu.xml file. 
Restart...

![PLDOCU SQL Developer Extension](sqldeveloper_extension.jpg)

## License
PLDocu is released under the [MIT license](https://github.com/teotiger/pldocu/blob/master/license.txt).

## Roadmap
* code review of the render_docu function
* create an example block from the example values
* support other subprograms (types) in package
* ...

## Version History
Version 0.3.1 – May 31, 2018
* Various improvements in RENDER_DOCU (reStructuredText as output format, translation of keywords, parameter section)
* Replacement of SYNTAX_INFOS with a new extended function SUBPROGRAM_ARGUMENT_INFOS
* APEX example application

Version 0.3 – May 23, 2018
* New (pipelined) function: SYNTAX_INFOS
* New function: RENDER_DOCU

Version 0.2 – April 15, 2018
* Extension for SQL Developer added

Version 0.1 – March 31, 2018
* Initial release
