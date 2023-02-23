
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

