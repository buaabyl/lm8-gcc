
// DO NOT EDIT THIS FILE - it is machine generated -*- c++ -*-

#ifndef __gnu_java_awt_peer_gtk_GtkMenuComponentPeer__
#define __gnu_java_awt_peer_gtk_GtkMenuComponentPeer__

#pragma interface

#include <gnu/java/awt/peer/gtk/GtkGenericPeer.h>
extern "Java"
{
  namespace gnu
  {
    namespace java
    {
      namespace awt
      {
        namespace peer
        {
          namespace gtk
          {
              class GtkMenuComponentPeer;
          }
        }
      }
    }
  }
  namespace java
  {
    namespace awt
    {
        class Font;
        class MenuComponent;
    }
  }
}

class gnu::java::awt::peer::gtk::GtkMenuComponentPeer : public ::gnu::java::awt::peer::gtk::GtkGenericPeer
{

public: // actually protected
  virtual void create() = 0;
private:
  void setFont();
public:
  GtkMenuComponentPeer(::java::awt::MenuComponent *);
  virtual void dispose();
  virtual void setFont(::java::awt::Font *);
  static ::java::lang::Class class$;
};

#endif // __gnu_java_awt_peer_gtk_GtkMenuComponentPeer__