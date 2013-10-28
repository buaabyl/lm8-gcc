
// DO NOT EDIT THIS FILE - it is machine generated -*- c++ -*-

#ifndef __java_text_spi_DateFormatProvider__
#define __java_text_spi_DateFormatProvider__

#pragma interface

#include <java/util/spi/LocaleServiceProvider.h>
extern "Java"
{
  namespace java
  {
    namespace text
    {
        class DateFormat;
      namespace spi
      {
          class DateFormatProvider;
      }
    }
  }
}

class java::text::spi::DateFormatProvider : public ::java::util::spi::LocaleServiceProvider
{

public: // actually protected
  DateFormatProvider();
public:
  virtual ::java::text::DateFormat * getDateInstance(jint, ::java::util::Locale *) = 0;
  virtual ::java::text::DateFormat * getDateTimeInstance(jint, jint, ::java::util::Locale *) = 0;
  virtual ::java::text::DateFormat * getTimeInstance(jint, ::java::util::Locale *) = 0;
  static ::java::lang::Class class$;
};

#endif // __java_text_spi_DateFormatProvider__