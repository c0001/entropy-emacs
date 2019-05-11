CREATE TABLE Prjs_Index(
    Prj_ID text,
    Prj_Name text,
    Prj_Author text,
    Prj_Date text,
    Prj_State
);

CREATE TABLE Prjs_Attr (
    Prj_ID text,
    Prj_Attr_Type text,
    Prj_Attr_Category text,
    Prj_Attr_Rfc text,
    Prj_Attr_Uri text,
    Prj_Attr_Des text
);

CREATE TABLE Prjs_Vcs (
    Prj_ID text,
    Prj_Vcs_Type text,
    Prj_Vcs_Remote text,
    Prj_Vcs_Status text,
    Prj_Vcs_Head text
);

CREATE TABLE DB_Version (
    Version text
);


INSERT INTO DB_Version (Version)
VALUES ('0.1.0');
