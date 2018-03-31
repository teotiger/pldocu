# PLDocu

## Introduction
PLDocu is a PL/SQL Package to extract comments for building a documentation.

## Installation
Simply run the install script from the setup folder inside SQL*Plus.

## Usage
The following example extract the information from package "PLDOCU":

```
select * 
from table(pldocu.package_infos('PLDOCU'))
natural join table(pldocu.subprogram_infos('PLDOCU'))
natural join table(pldocu.argument_infos('PLDOCU'))
order by sub_id, arg_id;
```

## License
PLDocu is released under the [MIT license](https://github.com/teotiger/pldocu/blob/master/license.txt).

## Version History
Version 0.1 – March 31, 2018
* Initial release

