# Native SQLite access for Clarion

## SQLite-x advantage vs FILE,DRIVER('sqlite')
- Old Clarion versions (C6,C7,C8) supported. SQLite driver was introduced only in C9.
- ClaLIT.dll not required for distribution.
- SQL access to non-SQL data (for example, TPS).
- No need to declare dummy table to obtain SQL query results.
- Prepared statements supported.
- Progress of query execution.
- SQL query result may be immediately stored in a variable, a group, a queue, or a table.
- You can apply SQL filter in standard Browse template, even against non-SQL data.  
For example, in a browse by TPS table you can apply such filter  

```
"WHERE person.Lastname LIKE 'c%' AND division.Number IN (3, 5)" 
```

- Arbitrary SQL queries support.
- UTF-8 support.

[Download the demo](https://yadi.sk/d/IEy1p9LnpPIePQ)  
**If you are not able to download from the link above, drop me private message and I will send you a zip.**   

## Requirements
- C6 and higher, ABC/Legacy
- source code only 

## Dependencies
- sqlite3.dll
 
## Price
- $100 [PayPal](https://www.paypal.me/mikeduglas?ppid=PPC000628&cnac=RU&rsta=ru_RU(ru_RU)&cust=8W29QJ6GKY9HS&unptid=75f96da6-24a4-11e9-ae2c-441ea14e9560&t=&cal=ff0291196b3f5&calc=ff0291196b3f5&calf=ff0291196b3f5&unp_tpcid=ppme-social-user-profile-created&page=main:email&pgrp=main:email&e=op&mchn=em&s=ci&mail=sys)