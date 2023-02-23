
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

CREATE OR REPLACE PROCEDURE CREAR_CAMPO_SI_NO_EXISTE 
   (NombreTabla     VARCHAR2,
    DefEJECUTAR_CON_LOGnCampo VARCHAR2,
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
   -- @DefEJECUTAR_CON_LOGnCampo : Es la defEJECUTAR_CON_LOGn del campo a crear, con la sintaxis:
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
   -- EXEC dbo.CREAR_CAMPO_SI_NO_EXISTE 'TABLAPRUEBA', 'CAMPONUEVO1, VARCHAR, , 10, NONULO'
   -- EXEC dbo.CREAR_CAMPO_SI_NO_EXISTE 'TABLAPRUEBA', 'CAMPONUEVO1, VARCHAR, VALOR DEF, 10, NONULO'
   -- EXEC dbo.CREAR_CAMPO_SI_NO_EXISTE 'TABLAPRUEBA', 'CAMPONUEVO2, INT'
   -- ------------------------------------------------------------------------------------------------------------
   SeAgregoCampo := 'N';
   NroAtt        := 1;
  
   IF EXISTE_TABLA(NombreTabla) = 1 THEN
        CAMPOS_CURSOR := SPLIT_STRING(DefEJECUTAR_CON_LOGnCampo, ',');   
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
      
      IF (NOT (EXISTE_CAMPO(NombreTabla, NombreCampo) = 1)) THEN
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
      DBMS_OUTPUT.PUT_LINE('Se omite creacion de campo [' || DefEJECUTAR_CON_LOGnCampo || '] ya que la tabla [' || NombreTabla || '] no existe.');
   END IF;
END;

--------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------

CREATE OR REPLACE PROCEDURE CREAR_CLAVE_FORANEA_SI_NO_EXISTE (
   NombreTablaPadre      VARCHAR2, 
   NombreClaveForanea    VARCHAR2, 
   ListaCamposTablaPadre VARCHAR2, 
   NombreTablaHija       VARCHAR2,
   ListaCamposTablaHija  VARCHAR2 
)
AS
BEGIN
   DECLARE
      Query VARCHAR2(4000);
   BEGIN
      -- ------------------------------------------------------------------------------------------------------------
      -- Crea una nueva clave foranea si no existe, asociado a la tabla padre indicada, y referenciando a la tabla 
      -- hija indicada a traves de los campos especificados.
      -- Parámetros:
      -- NombreTablaPadre      Nombre de la tabla principal (sobre la que se crea la FK) 
      -- NombreClaveForanea    Nombre de la clave foranea a crear 
      -- ListaCamposTablaPadre Lista de campos que componen la referencia (separados por coma) 
      -- NombreTablaHija       Nombre de la tabla hija
      -- ListaCamposTablaHija  Lista de campos que componen la referencia en la tabla hija (separados por coma) 
      -- Ejemplo de Uso:
      -- EXEC dbo.CREAR_CLAVE_FORANEA_SI_NO_EXISTE 'LINEAS', 'FK_CABEZAL', 'TIPO, SERIE, NUMERO', 
      --                                        'CABEZAL', 'TIPO, SERIE, NUMERO'  
      -- ------------------------------------------------------------------------------------------------------------
      IF (EXISTE_TABLA(NombreTablaPadre)=1) THEN
         IF (EXISTE_TABLA(NombreTablaHija)=1) THEN
            IF (EXISTE_CLAVE_FORANEA(NombreTablaPadre, NombreClaveForanea)=0) THEN 
               Query := 'ALTER TABLE '    || NombreTablaPadre      || ' ' ||
                        'ADD CONSTRAINT ' || NombreClaveForanea    || ' ' ||
                        'FOREIGN KEY ('   || ListaCamposTablaPadre || ')' || ' ' ||
                        'REFERENCES '     || NombreTablaHija       || ' ' ||
                        '(' || ListaCamposTablaHija || ' ) ';
      
               DBMS_OUTPUT.PUT_LINE(Query);
               EXECUTE IMMEDIATE Query;
            ELSE
               DBMS_OUTPUT.PUT_LINE('Se omite creación de clave foránea [' || NombreClaveForanea || '] porque ya existe.');
            END IF;
         ELSE
            DBMS_OUTPUT.PUT_LINE('Se omite creación de clave foránea [' || NombreClaveForanea || '] porque la tabla hija [' || NombreTablaHija || '] no existe');
         END IF;
      ELSE
         DBMS_OUTPUT.PUT_LINE('Se omite creación de clave foránea [' || NombreClaveForanea || '] porque la tabla padre [' || NombreTablaPadre || '] no existe');
      END IF;
   END;
END;

--------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------

CREATE OR REPLACE PROCEDURE CREAR_INDICE_SI_NO_EXISTE (
   NombreTabla  VARCHAR2, 
   NombreIndice VARCHAR2, 
   ListaCampos  VARCHAR2
)
AS
BEGIN
   DECLARE
      Query VARCHAR2(4000);
   BEGIN
      -- ------------------------------------------------------------------------------------------------------------
      -- Crea un nuevo indice si no existe, asociado a la tabla indicada, y con la especificación de campos dada.
      -- Parámetros:
      -- @NombreIndice      : Es el nombre del indice a crear.
      -- @NombreTabla       : Es el nombde de la tabla sobre la que se creara el indice.
      -- @ListaCampos       : Es la lista de nombres de campos que integran el indice, separados por coma.
      -- Ejemplo de Uso:
      -- EXEC dbo.CREAR_INDICE_SI_NO_EXISTE 'TABLAPRUEBA', 'IDX_PRUEBA_01', 'CAMPO1, CAMPO2, CAMPO3'  
      -- ------------------------------------------------------------------------------------------------------------
      IF (EXISTE_TABLA(NombreTabla)=1) THEN
         IF (EXISTE_INDICE(NombreIndice)=0) THEN 
            Query := 'CREATE INDEX ' || NombreIndice || 
                     ' ON ' || NombreTabla || ' ' ||
                     ' (' || ListaCampos || ' ) ';

            DBMS_OUTPUT.PUT_LINE(Query);
            EXECUTE IMMEDIATE Query;
         ELSE
            DBMS_OUTPUT.PUT_LINE('Se omite creación de índice [' || NombreIndice || '] porque ya existe.');
         END IF;
      ELSE
         DBMS_OUTPUT.PUT_LINE('Se omite creación de índice [' || NombreIndice || '] porque la tabla [' || NombreTabla || '] sobre la que se intenta crear no existe');
      END IF;
   END;
END;

--------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------

CREATE OR REPLACE PROCEDURE CREAR_TABLA_SI_NO_EXISTE ( 
   NombreTabla      VARCHAR2, 
   DefEJECUTAR_CON_LOGnCampos VARCHAR2, 
   ListaCamposPK    VARCHAR2
)
AS
BEGIN
   -- ------------------------------------------------------------------------------------------------------------
   -- Crea una nueva tabla si no existe, con la especificación de campos y la clave primaria indicada.
   -- Parámetros:
   -- @NombreTabla      : Es el nombre de la tabla a crear.
   -- @DefEJECUTAR_CON_LOGnCampos : Es la lista de campos a crear, con la siguiente sintaxis
   --                     <NombreCampo1>, <Tipo>, <Largo>, <NoNulo> |
   --                     <NombreCampo2>, <Tipo>, <Largo>, <NoNulo> |
   --                     ...
   --                     <NombreCampoN>, <Tipo>, <Largo>, <NoNulo>
   --
   --                     El separador de campos es "|" (pipe) y el de atributos es "," (coma)
   --                     Tipo corresponde a uno de los siguientes:
   --                     - varchar          cadena de caracteres, se debe indicar largo
   --                     - int              entero 10 digitos
   --                     - money            numerico de 16 enteros y 4 decimales
   --                     - decimal          numerico de 20 enteros y 8 decimales --> se traduce a numeric(28,8)
   --                     - text             texto largo (usado para almacenar más de 4000 caracteres)
   --                     - date             fecha y hora
   --                     - boolean          se traduce a varchar(1) donde se almacenará S o N
   --                     - bigint           entero de más de 10 digitos
   -- @ListaCamposPK    : Es la lista de nombres de campos que integran la clave primaria, separados por coma.
   -- Ejemplo de Uso:
   -- EXEC dbo.CREAR_TABLA_SI_NO_EXISTE 'TABLAPRUEBA', 
   --                                   'ID, int, ,nonulo|
   --                                    NOMBRE, varchar, 50, nonulo|
   --                                    DESCRIPCION, text',
   --                                   'ID'
   -- ------------------------------------------------------------------------------------------------------------
   DECLARE 
      SeparadorCampos    VARCHAR2(1);
      SeparadorAtributos VARCHAR2(1);

      Query              VARCHAR2(4000);
      QueryCampos        VARCHAR2(4000);
      DefCampo           VARCHAR2(1000);
      NroCampo           NUMBER;
      AttCampo           VARCHAR2(1000);
      NroAtt             NUMBER;
      NombreCampo        VARCHAR2(100);
      TipoCampo          VARCHAR2(100);
      LargoCampo         VARCHAR2(100);
      NoNuloCampo        VARCHAR2(100);
      CursorCampos       SYS_REFCURSOR;
      CursorDefCampo     SYS_REFCURSOR;
      NombrePK           VARCHAR2(4000);
   BEGIN
   
      IF (EXISTE_TABLA(NombreTabla)=0) THEN 
         SeparadorCampos    := '|';
         SeparadorAtributos := ',';
      
         NroCampo    := 1;
         QueryCampos := '';
      
         -- Recorrida de campos
         CursorCampos := SPLIT_STRING(DefEJECUTAR_CON_LOGnCampos, SeparadorCampos);  
         LOOP
            FETCH CursorCampos INTO DefCampo;
            EXIT WHEN CursorCampos%NOTFOUND;
            
            DefCampo := TRIM(DefCampo); 
            IF (DefCampo IS NOT NULL) THEN            
               IF (NroCampo > 1) THEN 
                  QueryCampos := QueryCampos || ', ';
               END IF;
         
               NroAtt       := 1;
               NombreCampo  := '';
               TipoCampo    := '';
               LargoCampo   := '';
               NoNuloCampo  := '';
         
               -- Recorrida de atributos de un campo
               CursorDefCampo := SPLIT_STRING(DefCampo, SeparadorAtributos);
               LOOP
                  FETCH CursorDefCampo INTO AttCampo;
                  EXIT WHEN CursorDefCampo%NOTFOUND;
                  
                  AttCampo := TRIM(AttCampo);
         
                  -- Atributo 1 = Nombre del campo
                  IF (NroAtt = 1) THEN  
                     NombreCampo := AttCampo;
                  -- Atributo 2 = Tipo del campo
                  ELSIF (NroAtt = 2) THEN
                     TipoCampo := UPPER(AttCampo);
                  -- Atributo 3 = Largo del campo
                  ELSIF (NroAtt = 3) THEN 
                     LargoCampo := UPPER(AttCampo);
                  -- Atributo 4 = Indicador de nonulo
                  ELSIF (NroAtt = 4) THEN 
                     NoNuloCampo := UPPER(AttCampo);
                  END IF;
         
                  NroAtt := NroAtt + 1;
               END LOOP;
               CLOSE CursorDefCampo;            
            
               QueryCampos := QueryCampos || NombreCampo; 
               QueryCampos := QueryCampos || ' ';
            
               IF (TipoCampo = 'VARCHAR') THEN 
                  QueryCampos := QueryCampos || 'VARCHAR2' || '(' || LargoCampo || ')';
               ELSIF (TipoCampo = 'INT') THEN 
                  QueryCampos := QueryCampos || 'NUMERIC(38,0)';
               ELSIF (TipoCampo = 'MONEY') THEN 
                  QueryCampos := QueryCampos || 'NUMERIC(38,4)';
               ELSIF (TipoCampo = 'DECIMAL') THEN 
                  QueryCampos := QueryCampos || 'NUMERIC(38,8)';
               ELSIF (TipoCampo = 'TEXT') THEN
                  QueryCampos := QueryCampos || 'CLOB';
               ELSIF ((TipoCampo = 'DATE') OR (TipoCampo = 'DATETIME')) THEN
                  QueryCampos := QueryCampos || 'DATE';
               ELSIF (TipoCampo = 'BOOLEAN') THEN
                  QueryCampos := QueryCampos || 'VARCHAR2(1)';
               ELSIF (TipoCampo = 'BIGINT') THEN 
                  QueryCampos := QueryCampos || 'NUMERIC(38,0)';
               ELSE 
                  QueryCampos := QueryCampos || TipoCampo;
               END IF;
            
               QueryCampos := QueryCampos || ' ';
         
               IF (NoNuloCampo = 'NONULO') THEN
                  QueryCampos := QueryCampos || 'NOT NULL';
               END IF;
                  
               NroCampo := NroCampo + 1;
            END IF;
         END LOOP;
         CLOSE CursorCampos;
      
         NombrePK := 'PK_' || NombreTabla;
         
         Query := 'CREATE TABLE ' || NombreTabla || 
                  ' (' || QueryCampos || ',' ||
                  ' CONSTRAINT ' || NombrePK || ' PRIMARY KEY ( ' || ListaCamposPK || ' ) ' ||
                  ' ) ';
      
         DBMS_OUTPUT.PUT_LINE(Query);
         EXECUTE IMMEDIATE Query;
      ELSE
         DBMS_OUTPUT.PUT_LINE( 'Se omite creación de tabla ' || NombreTabla || ' dado que la misma ya existe en la base de datos.' );
      END IF;
   END;
END;

--------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------

CREATE OR REPLACE PROCEDURE ELIMINAR_CAMPO_SI_EXISTE(
   NombreTabla VARCHAR2,
   NombreCampo VARCHAR2
)
AS
BEGIN
   DECLARE
      Query VARCHAR2(4000);
   BEGIN   
      -- ------------------------------------------------------------------------------------------------------------
      -- Elimina un campo en la tabla especificada, si el mismo existe
      -- Parámetros:
      -- @NombreTabla : Es el nombre de la tabla en el que se encuentra el campo a eliminar.
      -- @NombreCampo : Es el nombre del campo a eliminar
      -- Ejemplo de Uso:
      -- EXEC dbo.ELIMINAR_CAMPO_SI_EXISTE 'TABLAPRUEBA', 'CAMPO1'
      -- ------------------------------------------------------------------------------------------------------------
      
      IF (EXISTE_TABLA(NombreTabla)=1) THEN 
         -- Verificacion de existencia del campo a eliminar
         IF (EXISTE_CAMPO(NombreTabla, NombreCampo)=1) THEN
            Query := 'ALTER TABLE ' || NombreTabla || ' ' ||
                     'DROP COLUMN ' || NombreCampo;
      
            DBMS_OUTPUT.PUT_LINE(Query);
            EXECUTE IMMEDIATE Query;
         ELSE
            DBMS_OUTPUT.PUT_LINE('Se omite eliminación de campo [' || NombreCampo || '] porque no existe en la tabla [' || NombreTabla || '].');
         END IF;
      ELSE
         DBMS_OUTPUT.PUT_LINE('Se omite eliminación de campo [' || NombreCampo || '] porque la tabla [' || NombreTabla || '] no existe.');
      END IF;
   END;
END;

--------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------

CREATE OR REPLACE PROCEDURE MODIFICAR_TIPO_CAMPO_SI_EXISTE(
   NombreTabla VARCHAR2,
   NombreCampo VARCHAR2,
   TipoDato VARCHAR2
)
AS
BEGIN
   DECLARE
      Query VARCHAR2(4000);
      NombreCampoTemp VARCHAR2(200);
   BEGIN   
      -- ------------------------------------------------------------------------------------------------------------
      -- Cambia el tipo de datos de un campo en la tabla especificada, si el campo existe
      -- Parámetros:
      -- @NombreTabla : Es el nombre de la tabla en el que se encuentra el campo a eliminar.
      -- @NombreCampo : Es el nombre del campo a eliminar
      -- @TipoDato    : Nuevo tipo de dato, por ejemplo CLOB, VARCHAR2(2000), INT
      -- Ejemplo de Uso:
      -- EXEC dbo.MODIFICAR_TIPO_CAMPO_SI_EXISTE('TABLAPRUEBA', 'CAMPO1', 'CLOB')
      -- ------------------------------------------------------------------------------------------------------------
      
      IF (EXISTE_TABLA(NombreTabla)=1) THEN 
         -- Verificacion de existencia del campo a eliminar
         IF (EXISTE_CAMPO(NombreTabla, NombreCampo)=1) THEN
            -- ALTER TABLE <tabla> ADD (<campo>_TEMP <tipo>)
            -- UPDATE <tabla> SET <campo>_TEMP=<campo>
            -- ALTER TABLE <tabla> DROP COLUMN <campo>
            -- ALTER TABLE <tabla> RENAME COLUMN <campo>_TEMP TO <campo>

            NombreCampoTemp := NombreCampo || '_TEMP';
            
            -- Si el campo temporal existe, se elimina antes de continuar.
            IF (EXISTE_CAMPO(NombreTabla, NombreCampoTemp)=1) THEN
               Query := 'ALTER TABLE ' || NombreTabla || ' DROP COLUMN ' || NombreCampoTemp;
               DBMS_OUTPUT.PUT_LINE(Query);
               EXECUTE IMMEDIATE Query;
            END IF;

            -- Se crea campo temporal
            Query := 'ALTER TABLE ' || NombreTabla || ' ADD (' || NombreCampoTemp || ' ' || TipoDato || ')';
            DBMS_OUTPUT.PUT_LINE(Query);
            EXECUTE IMMEDIATE Query;
            
            -- Se copia valor de campo original al campo temporal
            Query := 'UPDATE ' || NombreTabla || ' SET ' || NombreCampoTemp || ' = ' || NombreCampo;
            DBMS_OUTPUT.PUT_LINE(Query);
            EXECUTE IMMEDIATE Query;

            -- Se elimina campo original
            Query := 'ALTER TABLE ' || NombreTabla || ' DROP COLUMN ' || NombreCampo;
            DBMS_OUTPUT.PUT_LINE(Query);
            EXECUTE IMMEDIATE Query;
            
            -- Se renombra campo temporal a campo original
            Query := 'ALTER TABLE ' || NombreTabla || ' RENAME COLUMN ' || NombreCampoTemp || ' TO ' || NombreCampo;
            DBMS_OUTPUT.PUT_LINE(Query);
            EXECUTE IMMEDIATE Query;

         ELSE
            DBMS_OUTPUT.PUT_LINE('Se omite modificacion de tipo de dato de campo [' || NombreCampo || '] porque no existe en la tabla [' || NombreTabla || '].');
         END IF;
      ELSE
         DBMS_OUTPUT.PUT_LINE('Se omite modificación de tipo de dato de campo [' || NombreCampo || '] porque la tabla [' || NombreTabla || '] no existe.');
      END IF;
   END;
END;

--------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------

CREATE OR REPLACE PROCEDURE ELIMINAR_CLAVE_FORANEA_SI_EXISTE(
   NombreTablaPadre   VARCHAR2,
   NombreClaveForanea VARCHAR2
)
AS
BEGIN
   DECLARE
      Query VARCHAR2(4000);
   BEGIN
      -- ------------------------------------------------------------------------------------------------------------
      -- Elimina la clave foranea especificada asociada a la tabla especificada, si la misma existe
      -- Parámetros:
      -- @NombreTablaPadre    : Es el nombre de la tabla sbre la que se encuentra la clave foranea a eliminar.
      -- @@NombreClaveForanea : Es el nombre de la clave foranea a eleiminar.
      -- Ejemplo de Uso:
      -- EXEC dbo.ELIMINAR_CLAVE_FORANEA_SI_EXISTE 'TABLAPRUEBA', 'FK_PRUEBA1'
      -- ------------------------------------------------------------------------------------------------------------
      IF (EXISTE_TABLA(NombreTablaPadre)=1) THEN
         IF (EXISTE_CLAVE_FORANEA(NombreTablaPadre, NombreClaveForanea)=1) THEN 
            Query := 'ALTER TABLE ' || NombreTablaPadre || ' ' ||
                     'DROP CONSTRAINT ' || NombreClaveForanea;
            DBMS_OUTPUT.PUT_LINE(Query);
            EXECUTE IMMEDIATE Query;
         ELSE
            DBMS_OUTPUT.PUT_LINE('Se omite eliminación de clave foranea [' || NombreClaveForanea || '] porque el mismo no existe en la base de datos.');
         END IF;
      ELSE
         DBMS_OUTPUT.PUT_LINE('Se omite eliminación de clave foranea [' || NombreClaveForanea || '] porque la tabla [' || NombreTablaPadre || '] asociada no existe.');
      END IF;
   END;
END;

--------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------

CREATE OR REPLACE PROCEDURE ELIMINAR_INDICE_SI_EXISTE( 
   NombreTabla  VARCHAR2,
   NombreIndice VARCHAR2
)
AS
BEGIN
   DECLARE
      Query VARCHAR2(4000);
   BEGIN
      -- ------------------------------------------------------------------------------------------------------------
      -- Elimina la clave foranea especificada asociada a la tabla especificada, si la misma existe
      -- Parámetros:
      -- @NombreTablaPadre    : Es el nombre de la tabla sbre la que se encuentra la clave foranea a eliminar.
      -- @@NombreClaveForanea : Es el nombre de la clave foranea a eleiminar.
      -- Ejemplo de Uso:
      -- EXEC dbo.ELIMINAR_INDICE_SI_EXISTE 'TABLAPRUEBA', 'FK_PRUEBA1'
      -- ------------------------------------------------------------------------------------------------------------
      IF (EXISTE_TABLA(NombreTabla)=1) THEN
         IF (EXISTE_INDICE(NombreIndice)=1) THEN 
            Query := 'DROP INDEX ' || NombreIndice;
            
            DBMS_OUTPUT.PUT_LINE(Query);
            EXECUTE IMMEDIATE Query;
         ELSE
            DBMS_OUTPUT.PUT_LINE('Se omite eliminación de indice [' || NombreIndice || '] porque el mismo no existe en la base de datos.');
         END IF;
      ELSE
         DBMS_OUTPUT.PUT_LINE('Se omite eliminación de indice [' || NombreIndice || '] porque la tabla [' || NombreTabla || '] asociada no existe.');
      END IF;
   END;
END;

--------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------

CREATE OR REPLACE PROCEDURE ELIMINAR_TABLA_SI_EXISTE (
   NombreTabla VARCHAR2
)
AS
BEGIN
   DECLARE 
      Query   VARCHAR2(4000);
   BEGIN
      -- ------------------------------------------------------------------------------------------------------------
      -- Elimina la tabla especificada, si la misma existe
      -- Parámetros:
      -- @NombreTabla  : Es el nombre de la tabla a eliminar.
      -- Ejemplo de Uso:
      -- EXEC dbo.ELIMINAR_TABLA_SI_EXISTE 'TABLAPRUEBA'
      -- ------------------------------------------------------------------------------------------------------------
      IF (EXISTE_TABLA(NombreTabla)=1) THEN
         Query := 'DROP TABLE ' || NombreTabla; 
      
         EXECUTE IMMEDIATE Query;
      ELSE
         DBMS_OUTPUT.PUT_LINE('Se omite eliminación de tabla ' || NombreTabla || ' dado que la misma no existe en la base de datos.');
      END IF;
   END;
END;

--------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------

CREATE OR REPLACE PROCEDURE RENOMBRAR_TABLA_SI_EXISTE (
   NombreTabla      VARCHAR2,
   NombreTablaNuevo VARCHAR2
)
AS
BEGIN
   DECLARE 
      Query VARCHAR2(4000);
   BEGIN
      IF (EXISTE_TABLA(NombreTabla)=1) THEN
         Query := 'ALTER TABLE ' || NombreTabla || ' RENAME TO ' || NombreTablaNuevo;
         DBMS_OUTPUT.PUT_LINE(Query);
         EXECUTE IMMEDIATE Query;

         IF (EXISTE_PK( 'PK_' || NombreTabla )=1) THEN
            Query := 'ALTER TABLE ' || NombreTablaNuevo || ' RENAME CONSTRAINT ' || 'PK_' || NombreTabla || ' TO ' || 'PK_' || NombreTablaNuevo;
            DBMS_OUTPUT.PUT_LINE(Query);
            EXECUTE IMMEDIATE Query;
         END IF;
         
         IF (EXISTE_INDICE( 'PK_' || NombreTabla )=1) THEN
            Query := 'ALTER INDEX ' || 'PK_' || NombreTabla || ' RENAME TO ' || 'PK_' || NombreTablaNuevo;
            DBMS_OUTPUT.PUT_LINE(Query);
            EXECUTE IMMEDIATE Query;
         END IF;
      ELSE
         DBMS_OUTPUT.PUT_LINE('Se omite renombrado de tabla ' || NombreTabla || ' dado que la misma no existe en la base de datos.');
      END IF;
   END;
END;

--------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------

CREATE OR REPLACE PROCEDURE LOG_PROCESO(
   vMensaje   VARCHAR2,
   vIdProceso VARCHAR2
)
IS
   existetabla NUMBER;
   sentencia VARCHAR2(4000);
BEGIN
   SELECT COUNT(*) INTO existetabla FROM USER_TABLES WHERE UPPER(TABLE_NAME) = UPPER('TMPLOG_PROCESO');
   IF (existetabla = 0) THEN
      sentencia := 'CREATE TABLE "TMPLOG_PROCESO"
                      (
                         ID        NUMBER(38,0) NOT NULL,
                         Fecha     DATE NOT NULL,
                         PID       NUMBER(38,0),
                         IdProceso VARCHAR2(200),
                         Mensaje   VARCHAR2(4000),
                         CONSTRAINT "TMPLOG_PROCESO_Pk" PRIMARY KEY (ID)
                      )';
      DBMS_OUTPUT.PUT_LINE('query:' || sentencia);
      EXECUTE IMMEDIATE sentencia;
   END IF;

   sentencia := 'INSERT INTO "TMPLOG_PROCESO" (ID, Fecha, PID, IdProceso, Mensaje) 
                 SELECT ((SELECT NVL(MAX(ID),0) FROM "TMPLOG_PROCESO")+1) AS ID, 
                        SYSDATE AS Fecha, 
                        SYS_CONTEXT(''USERENV'', ''SESSIONID'') AS PID, 
                        vIdProceso AS IdProceso, 
                        vMensaje AS Mensaje 
                 FROM DUAL';
   EXECUTE IMMEDIATE sentencia;
END;

--------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------
CREATE OR REPLACE PROCEDURE EJECUTAR_CON_LOG (
   Query     VARCHAR2,
   GrabarLog NUMBER,
   IdProceso VARCHAR2
)
IS
BEGIN
   DECLARE
     Inicio                DATE;
     Fin                   DATE;
     RowCount              NUMBER;
     SegundosTranscurridos NUMBER;
     Mensaje               VARCHAR(4000) := '';

   BEGIN
      IF (GrabarLog = 1) THEN
         LOG_PROCESO(Query, IdProceso);
      END IF;

      Inicio := SYSDATE;

      DBMS_OUTPUT.PUT_LINE('Query: ' || Query);
      EXECUTE IMMEDIATE Query;
   
      RowCount := SQL%ROWCOUNT;
      Fin  := SYSDATE;
      
      SELECT TRUNC(24 * 60 * 60 * (Fin - Inicio)) INTO SegundosTranscurridos FROM DUAL;
      
      IF (GrabarLog = 1) THEN
         Mensaje := 'OK. Filas afectadas: ' || TO_CHAR(RowCount) || '. ' ||
                    'Tiempo insumido: ' || TO_CHAR(TRUNC(SegundosTranscurridos)) || ' segundos.';
         DBMS_OUTPUT.PUT_LINE(Mensaje);
         LOG_PROCESO(Mensaje, IdProceso);
      END IF;
   EXCEPTION
      WHEN OTHERS THEN
         IF (GrabarLog = 1) THEN
            Mensaje := '>> Error: ' || SQLCODE || ' -- ' || SQLERRM;
            DBMS_OUTPUT.PUT_LINE(Mensaje);
            LOG_PROCESO(Mensaje, IdProceso);
         ELSE
            RAISE;
         END IF;
   END;
END;

--------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------

CREATE OR REPLACE PROCEDURE COPIAR_TABLA(
      TablaOriginal     VARCHAR2,
      TablaDestino      VARCHAR2,
      SchemaOriginal    VARCHAR2,  
      SchemaDestino     VARCHAR2,
      RecrearSiExiste   NUMBER,
      IdProceso         VARCHAR2,
      GrabarLog         NUMBER)
AS
BEGIN
   DECLARE
      existePk        NUMBER;
      query           VARCHAR2(4000);
      sigo            NUMBER;
      pkNombre        VARCHAR2(4000);
      pkColumnas      VARCHAR2(4000);
      vSchemaOriginal VARCHAR2(4000);
      vSchemaDestino  VARCHAR2(4000);
      mensaje         VARCHAR2(4000);
   BEGIN
      vSchemaOriginal := SchemaOriginal;
      IF (vSchemaOriginal IS NULL) THEN
         SELECT sys_context( 'userenv', 'current_schema' ) INTO vSchemaOriginal FROM DUAL;
      END IF;
      
      vSchemaDestino := SchemaDestino;
      IF (vSchemaDestino IS NULL) THEN
         SELECT sys_context( 'userenv', 'current_schema' ) INTO vSchemaDestino FROM DUAL;
      END IF;
      
      sigo := 1;
      IF (EXISTE_TABLA(TablaOriginal) = 1) THEN
         IF (EXISTE_TABLA(TablaDestino) = 1) THEN
            IF (RecrearSiExiste = 1) THEN 
               query := 'DROP TABLE "' || vSchemaDestino || '"."' || TablaDestino || '"';
               EJECUTAR_CON_LOG( query, GrabarLog, IdProceso );
            ELSE
               IF (GrabarLog = 1) THEN
                  LOG_PROCESO('La tabla destino "' || TablaDestino || '" ya existe y no se especificó RecrearSiExiste, se omite copiado de tabla.', IdProceso);
                  sigo := 0;
               END IF;
            END IF;
         END IF;
         
         IF (sigo = 1) THEN
            -- Copia de tabla (sin PK)
            query := 'CREATE TABLE "' || vSchemaDestino || '"."' || TablaDestino || '" AS ' ||
                     'SELECT * FROM "' || vSchemaOriginal || '"."' || TablaOriginal || '"';
            EJECUTAR_CON_LOG( query, GrabarLog, IdProceso );
            
            -- Creacion de PK (si la tabla origen tiene).
            SELECT COUNT(*) 
            INTO   existePk 
            FROM   all_constraints cons
            WHERE  cons.table_name = TablaOriginal
            AND    cons.constraint_type = 'P'
            AND    cons.owner = vSchemaOriginal;
            
            IF (existePk > 0) THEN
               pkNombre    := 'PK_' || TablaDestino;
               pkColumnas := '';
               
               -- Campos de la PK
               FOR cur_rec IN (
                  SELECT cols.column_name
                  FROM   all_constraints cons, all_cons_columns cols
                  WHERE  cols.table_name = TablaOriginal
                  AND    cons.owner      = vSchemaOriginal
                  AND    cons.constraint_type = 'P'
                  AND    cons.constraint_name = cols.constraint_name
                  AND    cons.owner = cols.owner
                  ORDER BY cols.table_name, cols.position) 
               LOOP
                  pkColumnas := pkColumnas || ',' || cur_rec.column_name;
               END LOOP;
               pkColumnas := LTRIM(pkColumnas, ',');
               
               -- Creacion de PK.
               query := 'ALTER TABLE "' || vSchemaDestino || '"."' || TablaDestino || '" ' ||
                        'ADD CONSTRAINT ' || pkNombre || ' PRIMARY KEY ( ' || pkColumnas || ' )';
               EJECUTAR_CON_LOG( query, GrabarLog, IdProceso );
            END IF;
         END IF;
      ELSE
         IF (GrabarLog = 1) THEN
            LOG_PROCESO('No existe la tabla origen "' || TablaOriginal || '", se omite copiado de tabla.', IdProceso);
         END IF;
      END IF;
   END;
END;