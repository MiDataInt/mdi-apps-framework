## S3 classes

MDI **classes** defines the object oriented programming (OOP)
components of the Stage 1 apps framework.

In general, scripts define functions that create S3 classes.

Constructor functions follow suggested R best practices in being
called 'new_myClass' by convention. Note the case pattern.

Because R objects are never accessed directly by end
users (only by developers writing framework code), there is
generally no need to export validation or helper functions. 

Objects defined by classes are never used to access or create
the UI, only to streamline code development by encapsulating 
methods and other common logic. 

By convention, a class is defined by scripts named:
    
- **myClass_constructor.R** - for object instantiation
- **myClass_methods.R** - where generic S3 methods are found
- **myClass_utilities.R** - functions called by the above scripts
