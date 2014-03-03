from django.conf.urls import patterns, include, url

#from django.contrib import admin
#admin.autodiscover()

from chris_martin_org.views import HomeView

urlpatterns = patterns('',
    # Examples:
    # url(r'^blog/', include('blog.urls')),
    # url(r'^admin/', include(admin.site.urls)),

    url(r'^$', HomeView.as_view(), name='home'),
)
