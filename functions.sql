
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

--------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------

CREATE OR REPLACE PROCEDURE Crear_Campo_Si_No_Existe 
   (NombreTabla     VARCHAR2,
    DefinicionCampo VARCHAR2,
    SeAgregoCampo   OUT VARCHAR2
   )
IS
   QueryCampos VARCHAR(10000);
   QueryUpdate VARCHAR(10000);
   Query       VARCHAR(10000);
   AttCampo    VARCHAR(10000);
   NroAtt      NUMBER;
   NombreCampo VARCHAR(100);
   ValorPorDef VARCHAR(100);
   TipoCampo   VARCHAR(100);
   LargoCampo  VARCHAR(100);
   NoNuloCampo VARCHAR(100);
   CAMPOS_CURSOR SYS_REFCURSOR;
   Sigo INT;
BEGIN
   -- ------------------------------------------------------------------------------------------------------------
   -- Crea un nuevo campo en la tabla especificada, si el mismo no existe
   -- Parámetros:
   -- @NombreTabla     : Es el nombre de la tabla donde se creara el nuevo campo.
   -- @DefinicionCampo : Es la definicion del campo a crear, con la sintaxis:
   --                    <NombreCampo>, <Tipo>, <ValorPorDefecto>, <Largo>, <NoNulo>
   --                     Tipo corresponde a uno de los siguientes:
   --                     - varchar          cadena de caracteres, se debe indicar largo
   --                     - int              entero 10 digitos
   --                     - money            numerico de 16 enteros y 4 decimales
   --                     - decimal          numerico de 20 enteros y 8 decimales --> se traduce a numeric(28,8)
   --                     - text             texto largo (usado para almacenar más de 4000 caracteres)
   --                     - date             fecha y hora
   --                     - boolean          se traduce a varchar(1) donde se almacenará S o N
   --                     - bigint           entero de más de 10 digitos
   --                     Ej de uso para ValorPorDefecto:
   --                     - Texto               text
   --                     - 200                 int, money, decimal
   --                     - 2016-10-23 20:44:11 fecha y hora
   --                     - S o N               boolean          
   -- Ejemplo de Uso:
   -- EXEC dbo.Crear_Campo_Si_No_Existe 'TABLAPRUEBA', 'CAMPONUEVO1, VARCHAR, , 10, NONULO'
   -- EXEC dbo.Crear_Campo_Si_No_Existe 'TABLAPRUEBA', 'CAMPONUEVO1, VARCHAR, VALOR DEF, 10, NONULO'
   -- EXEC dbo.Crear_Campo_Si_No_Existe 'TABLAPRUEBA', 'CAMPONUEVO2, INT'
   -- ------------------------------------------------------------------------------------------------------------
   SeAgregoCampo := 'N';
   NroAtt        := 1;
  
   IF Existe_Tabla(NombreTabla) = 1 THEN
        CAMPOS_CURSOR := SPLIT_STRING(DefinicionCampo, ',');   
        LOOP
            FETCH CAMPOS_CURSOR INTO AttCampo;
            EXIT WHEN CAMPOS_CURSOR%NOTFOUND;
            
            AttCampo := trim(AttCampo);
           
            -- Atributo 1 = Nombre del campo
            IF (NroAtt = 1) THEN  
               NombreCampo := AttCampo;
            -- Atributo 2 = Tipo del campo
            ELSIF (NroAtt = 2) THEN
               TipoCampo := UPPER(AttCampo);
            -- Atributo 3 = Valor por defecto
            ELSIF (NroAtt = 3) THEN 
              ValorPorDef := upper(AttCampo);
            -- Atributo 3 = Largo del campo
            ELSIF (NroAtt = 4) THEN 
               LargoCampo := upper(AttCampo);
            -- Atributo 4 = Indicador de nonulo
            ELSIF (NroAtt = 5) THEN  
               NoNuloCampo := upper(AttCampo);
            END IF;

            NroAtt := NroAtt + 1;
            
        END LOOP;
        CLOSE CAMPOS_CURSOR;
        
      QueryCampos := QueryCampos || NombreCampo ;
      QueryCampos := QueryCampos || ' ';
      
      DBMS_OUTPUT.PUT_LINE('ValorPorDef>>>>' || ValorPorDef);
      
      QueryUpdate := '';
      IF (ValorPorDef IS NOT NULL) THEN  
         QueryUpdate := QueryUpdate || NombreCampo || '='; 
      END IF; 
      
      IF (TipoCampo = 'VARCHAR') THEN
         QueryCampos := QueryCampos || 'VARCHAR2' || '(' || LargoCampo || ')';
         IF (ValorPorDef IS NOT NULL) THEN
            QueryUpdate := QueryUpdate || '''' || ValorPorDef || '''';
         END IF;   
      ELSIF (TipoCampo = 'INT' ) THEN
         QueryCampos := QueryCampos || 'NUMERIC(38,0)';
         IF (ValorPorDef IS NOT NULL) THEN       
            QueryUpdate := QueryUpdate || ValorPorDef; 
         END IF;
      ELSIF (TipoCampo = 'MONEY' ) THEN
         QueryCampos := QueryCampos || 'NUMERIC(38,4)';
         IF (ValorPorDef IS NOT NULL) THEN
            QueryUpdate := QueryUpdate || ValorPorDef;
        END IF;
      ELSIF (TipoCampo = 'DECIMAL') THEN 
         QueryCampos := QueryCampos || 'NUMERIC(38,8)';
         IF (ValorPorDef IS NOT NULL) THEN                   
            QueryUpdate := QueryUpdate || ValorPorDef;
         END IF;
      ELSIF (TipoCampo = 'TEXT' ) THEN
         QueryCampos := QueryCampos || 'CLOB';
         IF (ValorPorDef IS NOT NULL) THEN         
            QueryUpdate := QueryUpdate  || '''' || ValorPorDef || '''';
         END IF;     
      ELSIF ((TipoCampo = 'DATE') OR (TipoCampo = 'DATETIME')) THEN
         QueryCampos := QueryCampos || 'DATE';
         IF (ValorPorDef IS NOT NULL) THEN
            QueryUpdate := QueryUpdate  || 'TO_DATE(' || '''' || ValorPorDef || '''' || ', ' || '''YYYY-MM-DD HH24:MI:SS''' || ')'; 
         END IF;   
      ELSIF (TipoCampo = 'BOOLEAN') THEN
         QueryCampos := QueryCampos || 'VARCHAR2(1)';
         IF (ValorPorDef IS NOT NULL) THEN
            QueryUpdate := QueryUpdate  || '''' || ValorPorDef || '''';
         END IF;
      ELSIF (TipoCampo = 'BIGINT' ) THEN
         QueryCampos := QueryCampos || 'NUMERIC(38,0)';
         IF (ValorPorDef IS NOT NULL) THEN       
            QueryUpdate := QueryUpdate || ValorPorDef; 
         END IF;      
      ELSE 
         QueryCampos := QueryCampos || TipoCampo;         
      END IF;

      DBMS_OUTPUT.PUT_LINE('QueryUpdate>>>>' || QueryUpdate);
      
      IF (NOT (Existe_Campo(NombreTabla, NombreCampo) = 1)) THEN
         SeAgregoCampo := 'S';
         Query := 'ALTER TABLE ' || NombreTabla || ' ' ||
                  'ADD ' || QueryCampos; 

         DBMS_OUTPUT.PUT_LINE(Query);
         EXECUTE IMMEDIATE Query;
         
         if (QueryUpdate IS NOT NULL) THEN
            -- El update se hace incremental (de a bloques) para evitar que se llene el log en inicilizaciones de campos
            -- sobre tablas muy voluminosas
            Sigo := 1;
            WHILE Sigo > 0
            LOOP
            
               Query := 'UPDATE ' || NombreTabla || ' ' ||
                        'SET '    || QueryUpdate || ' ' ||
                        'WHERE '  || NombreCampo || ' IS NULL ' ||
                        'AND ROWNUM <= 10000';

               -- Se hace update del bloque y se comitea
               --BEGIN TRAN
               DBMS_OUTPUT.PUT_LINE(Query);
               EXECUTE IMMEDIATE Query;
               --COMMIT
               
               -- Si ya no quedan registros por actualizar, se termina el loop
               IF (SQL%ROWCOUNT <= 0) THEN
                    Sigo := 0;
               END IF; 
            END LOOP;
         END IF;
      ELSE
         SeAgregoCampo := 'N';
         DBMS_OUTPUT.PUT_LINE('Se omite creacion de campo [' || NombreCampo || '] porque ya existe en la tabla [' || NombreTabla || '].');      
      END IF;
      
   ELSE   
      DBMS_OUTPUT.PUT_LINE('Se omite creacion de campo [' || DefinicionCampo || '] ya que la tabla [' || NombreTabla || '] no existe.');
   END IF;
END;

