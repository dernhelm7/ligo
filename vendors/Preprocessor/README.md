# A preprocessor a la C# in OCaml

The following preprocessing directives are supported
  * #define
  * #elif
  * #else
  * #endif
  * #endregion
  * #error
  * #if
  * #include
  * #region
  * #undef

Note: There is no error raised for invalid preprocessing directives
(they are simply ignored).