
CREATE OR REPLACE FUNCTION EXISTE_FUNCION(NOMBRE_FUNCION VARCHAR2) RETURN NUMBER
IS                                                                                                                    
   RETORNO NUMBER;
BEGIN
   RETORNO := 0;
   SELECT COUNT(*) INTO RETORNO 
   FROM ALL_ARGUMENTS
   WHERE UPPER(OBJECT_NAME) =  UPPER(NOMBRE_FUNCION);
   
   IF (RETORNO >= 1) THEN
      RETORNO := 1;
   END IF;
   
   RETURN RETORNO;
END;

--------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------

CREATE OR REPLACE FUNCTION EXISTE_PROCEDIMIENTO(NOMBRE_PROCEDIMIENTO VARCHAR2) RETURN NUMBER
IS                                                                                                                    
   RETORNO NUMBER;
BEGIN
   RETORNO := 0; 
   SELECT COUNT(*) INTO RETORNO
   FROM USER_SOURCE 
   WHERE TYPE='PROCEDURE' 
   AND UPPER(NAME) = UPPER(NOMBRE_PROCEDIMIENTO);
   
   IF (RETORNO >= 1) THEN
      RETORNO := 1;
   END IF;
   
   RETURN RETORNO;
END;

--------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------

CREATE OR REPLACE PROCEDURE ELIMINAR_FUNCION_SI_EXISTE(NOMBRE_FUNCION IN VARCHAR2)
IS
   QUERY VARCHAR(200);
BEGIN    
   IF (EXISTE_FUNCION(NOMBRE_FUNCION) = 1) THEN
      QUERY := CONCAT('DROP FUNCTION ',  UPPER(NOMBRE_FUNCION));
      DBMS_OUTPUT.PUT_LINE(QUERY);
      EXECUTE IMMEDIATE QUERY;
   ELSE      
      DBMS_OUTPUT.PUT_LINE(CONCAT(CONCAT('SE OMITE ELIMINACIÓN DE FUNCIÓN [', NOMBRE_FUNCION),  '] PORQUE NO EXISTE'));
   END IF;
END;

--------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------

CREATE OR REPLACE PROCEDURE ELIMINAR_PROCEDIMIENTO_SI_EXISTE(NOMBRE_PROCEDIMIENTO IN VARCHAR2)
IS
   QUERY VARCHAR(100);
BEGIN    
   IF (EXISTE_PROCEDIMIENTO(NOMBRE_PROCEDIMIENTO) = 1) THEN
      QUERY := CONCAT('DROP PROCEDURE ',  UPPER(NOMBRE_PROCEDIMIENTO));
      DBMS_OUTPUT.PUT_LINE(QUERY);
      EXECUTE IMMEDIATE QUERY;
   ELSE      
      DBMS_OUTPUT.PUT_LINE(CONCAT(CONCAT('SE OMITE ELIMINACIÓN DEL PROCEDIMIENTO [', NOMBRE_PROCEDIMIENTO),  '] PORQUE NO EXISTE'));
   END IF;
END;

--------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------

CREATE OR REPLACE FUNCTION SPLIT_STRING(TEXTO CLOB, DELIMITADOR VARCHAR2) RETURN SYS_REFCURSOR
IS
BEGIN
   DECLARE
      RETORNO SYS_REFCURSOR;
      QUERY  CLOB;
      EXPR1  CLOB;
      EXPR2  CLOB;
      vTEXTO CLOB;
   BEGIN
      vTEXTO := TEXTO;
      IF (DELIMITADOR != CHR(9)) THEN
         vTEXTO := TRIM(REPLACE(vTEXTO, CHR(9), ''));
      END IF;
      IF (DELIMITADOR != CHR(10)) THEN
         vTEXTO := TRIM(REPLACE(vTEXTO, CHR(10), ''));
      END IF;
      IF (DELIMITADOR != CHR(13)) THEN
         vTEXTO := TRIM(REPLACE(vTEXTO, CHR(13), ''));
      END IF;

      EXPR1 := '(.*?)(\__DELIMITADOR__|$)';
      EXPR2 := '\__DELIMITADOR__';

      EXPR1 := REPLACE(EXPR1, '__DELIMITADOR__', DELIMITADOR);
      EXPR2 := REPLACE(EXPR2, '__DELIMITADOR__', DELIMITADOR);

      QUERY := 
         'SELECT REPLACE(REGEXP_SUBSTR( :vTEXTO, :EXPR1, 1, LEVEL ), :DELIMITADOR ) CAMPO ' ||
         'FROM DUAL ' ||
         'CONNECT BY LEVEL <= REGEXP_COUNT( :vTEXTO, :EXPR2) + 1';

      OPEN RETORNO FOR QUERY USING vTEXTO, EXPR1, DELIMITADOR, vTEXTO, EXPR2;    
      RETURN RETORNO;
   END;
END;

--------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------

CREATE OR REPLACE FUNCTION EXISTE_CAMPO(TABLA VARCHAR2, CAMPO VARCHAR2) RETURN NUMBER
IS
   RETORNO NUMBER;
BEGIN
   RETORNO := 0; 
   SELECT COUNT(*) INTO RETORNO
   FROM USER_TAB_COLUMNS
   WHERE UPPER(COLUMN_NAME) = UPPER(CAMPO)
   AND   UPPER(TABLE_NAME)  = UPPER(TABLA);   
   
   IF (RETORNO >= 1) THEN
      RETORNO := 1;
   END IF;
   RETURN RETORNO;
END;


--------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------

CREATE OR REPLACE FUNCTION EXISTE_CLAVE_FORANEA(TABLA VARCHAR2, CLAVE VARCHAR2) RETURN NUMBER
IS
   RETORNO NUMBER;
BEGIN
   RETORNO := 0; 
   SELECT COUNT(*) INTO RETORNO
   FROM ALL_CONSTRAINTS
   WHERE TABLE_NAME    = TABLA
   AND CONSTRAINT_NAME = CLAVE;
   
   IF (RETORNO >= 1) THEN
      RETORNO := 1;
   END IF;
   RETURN RETORNO;
END;

--------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------

CREATE OR REPLACE FUNCTION EXISTE_INDICE(INDICE VARCHAR) RETURN NUMBER
IS
   RETORNO NUMBER;
BEGIN
   RETORNO := 0; 
   SELECT COUNT(*) INTO RETORNO
   FROM USER_INDEXES 
   WHERE INDEX_NAME = INDICE;
   
   IF (RETORNO >= 1) THEN
      RETORNO := 1;
   END IF;
   RETURN RETORNO;
END;

--------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------

CREATE OR REPLACE FUNCTION EXISTE_PK(PK VARCHAR) RETURN NUMBER
IS
   RETORNO NUMBER;
BEGIN
   RETORNO := 0; 
   SELECT COUNT(*) INTO RETORNO
   FROM  USER_CONSTRAINTS 
   WHERE CONSTRAINT_NAME = PK
   AND   CONSTRAINT_TYPE = 'P';
   
   IF (RETORNO >= 1) THEN
      RETORNO := 1;
   END IF;
   RETURN RETORNO;
END;

--------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------

CREATE OR REPLACE PROCEDURE ELIMINAR_PK(TABLA VARCHAR2) 
IS
BEGIN
   DECLARE 
      QUERY VARCHAR2(1000);
      NOMBRE_PK VARCHAR2(1000);
   BEGIN   
      NOMBRE_PK := 'PK_' || TABLA;
      IF EXISTE_PK(NOMBRE_PK) = 1 THEN
         SELECT 'ALTER TABLE ' || TABLA || ' DROP CONSTRAINT ' || CONSTRAINT_NAME INTO QUERY FROM ALL_CONSTRAINTS WHERE TABLE_NAME = TABLA AND CONSTRAINT_TYPE = 'P' AND ROWNUM = 1;
         EXECUTE IMMEDIATE QUERY;
      ELSE
         DBMS_OUTPUT.PUT_LINE(CONCAT(CONCAT('SE OMITE ELIMINACIÓN DE PK [', NOMBRE_PK), '] PORQUE NO EXISTE'));
      END IF;
   END;
END;

--------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------

CREATE OR REPLACE FUNCTION EXISTE_TABLA(TABLA VARCHAR2) RETURN NUMBER
IS
   RETORNO NUMBER;
BEGIN
   RETORNO := 0; 
   SELECT COUNT(*) INTO RETORNO
   FROM USER_TABLES 
   WHERE UPPER(TABLE_NAME) = UPPER(TABLA);
   
   IF (RETORNO >= 1) THEN
      RETORNO := 1;
   END IF;
   RETURN RETORNO;
END;


