## Understanding switch statements in jsligo

### Cases to test

1. 0 cases 0 default - not possible - syntax error
2. 1 case 0 default 
3. n cases 0 default
4. 0 cases 1 default
5. 1 case 1 default
6. n cases 1 default
7. More than one default is not allowed - syntax error

### A case can
1. fallthrough (no break;)
2. break
3. return
 
### A default can
1. break
2. return  

### There are 2 major scenarios to handle
1. case - case (9 sub scenarios)
2. case - default  (6 sub scenarios)

### Case - Case 
1. fallthrough
```
case X : 
  /* code 1 */
case Y : 
  /* code 2 */
/* rest of the code */
```
as
```
if (X) {
  /* code 1 */
} else {
  unit;
}
if (X || Y) {
  /* code 2 */
} else {
  unit;
}
/* rest of the code */
```
2. break
```
case X : 
  /* code 1 */
  break;
case Y :
  /* code 2 */
/* rest of the code */
```
as
```
if (X) {
  /* code 1 */
} else {
  unit;
}
if (Y) {
  /* code 2 */
} else {
  unit;
}
/* rest of the code */
```
3. return
```
case X : 
  /* code 1 */
  return V1;
case Y : 
  /* code 2 */
/* rest of the code */
```
as
```
if (X) {
  /* code 1 */
  return V1;
} else {
  if (Y) {
    /* code 2 */
  } else {
    unit;
  }
  /* rest of the code */
}
```
4. fallthrough - break
```
case X :
	/* code 1 */
case Y : 
	/* code 2 */
	break;
/* rest of the code */
```
as
```
if (X) {
	/* code 1 */
} else {
	unit;
}
if (X || Y) {
	/* code 2 */
} else {
	unit;
}
/* rest of the code */
```
5. break - break
```
case X : 
	/* code 1 */
	break;
case Y :
	/* code 2 */
	break;
/* rest of the code */
```
as
```
if (X) {
	/* code 1 */
} else {
	unit;
}
if (Y) {
	/* code 2 */
} else {
	unit;
}
/* rest of the code */
```
6. return - break
```
case X :
	/* code 1 */
	return V1
case Y :
	/* code 2 */
	break;
/* rest of the code */
```
as
```
if (X) {
	/* code 1 */
	return V1;
} else {
	if (Y) {
		/* code 2 */
	} else {
		unit;
	}
	/* rest of the code */
}
```
7. fallthrough - return
```
case X :
	/* code 1 */
case Y : 
	/* code 2 */
	return V2;
/* rest of the code */
```
as
```
if (X) {
	/* code 1 */
} else {
	unit;
}
if (X || Y) {
	/* code 2 */
	return V2;
} else {
	/* rest of the code */
}
```
8.  break - return
```
case X : 
	/* code 1 */
	break;
case Y :
	/* code 2 */
	return V2;
/* rest of the code */
```
as
```
if (X) {
	/* code 1 */
} else {
	unit;
}
if (Y) {
	/* code 2 */
	return V2;
} else {
	/* rest of the code */
}
```
9. return - return
```
case X :
	/* code 1 */
	return V1;
case Y :
	/* code 2 */
	return V2;
/* rest of the code */
``` 
as
```
if (X) {
	/* code 1 */
	return V1;
} else {
	if (Y) {
		/* code 2 */
		return V2;
	} else {
		/* rest of the code */
	}
}
```
### Case - Default
1. fallthrough - break
```
case X : 
	/* code 1 */
default : 
	/* code 2 */
/* rest of the code */
```
as
```
if (X) {
	/* code 1 */
} else {
	unit;
}
/* code 2 */ 

/* rest of the code */
```
2. break - break
```
case X :
	/* code 1 */
	break;
default :
	/* code 2 */
/* rest of the code */
```
as
```
if (X) {
	/* code 1 */
} else {
	/* code 2 */
}
/* rest of the code */
```
3. return - break
```
case X : 
	/* code 1 */
	return V1;
default : 
	/* code 2 */
/* rest of the code */
```
as
```
if (X) {
	/* code 1 */
	return V1;
} else {
	/* code 2 */
	
	/* rest of the code */
}
```
4. fallthrough - return
```
case X : 
	/* code 1 */
default :
	/* code 2 */
	return V2;
/* rest of the code */
```
as
```
if (X) {
	/* code 1 */
} else {
	unit;
}
/* code 2 */
return V2;
/* rest of the code will be ignored as we return V2 */
```
5. break - return
```
case X :
	/* code 1 */
	break;
default : 
	/* code 2 */
	return V2;
/* rest of the code */
```
as
```
if (X) {
	/* code 1 */
	
	/* rest of the code */
} else {
	/* code 2 */
	return V2;
}
```
6. return -return
```
case X : 
	/* code 1 */
	return V1;
default : 
	/* code 2 */
	return V2;
/* rest of the code */
```
as
```
if (X) {
	/* code 1 */
	return V1;
} else {
	/* code 2 */
	return V2;
} 
```
### Single Case
1. fallthrough
```
case X : 
	/* code 1 */
}
/* rest of the code */
```
as
```
if (X) {
	/* code 1 */
} else {
	unit;
}
/* rest of the code */
```
2. break
```
case X :
	/* code 1 */
	break;
}
/* rest of the code */
```
as
```
// same as above 
if (X) {
	/* code 1 */
} else {
	unit;
}
/* rest of the code */
```
3. return
```
case X :
	/* code 1 */
	return V1;
}
/* rest of the code */
```
as
```
if (X) {
	/* code 1 */
	return V1;
} else {
	/* rest of the code */
}
```

### Single Default
1. break
```
// No cases above
default : 
	/* code 1 */
}
/* rest of the code */
```
as 
```
/* code 1 */

/* rest of the code */
```
2. return
```
// No cases above
default :
	/* code 1 */
	return V1;
}
/* rest of the code */
```
as
```
/* code 1 */
return V1;
/* rest of the code will be ignored as we return V1 */
```
