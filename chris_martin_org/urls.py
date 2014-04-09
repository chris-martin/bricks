from django.conf.urls import patterns, url

from django.contrib import admin
admin.autodiscover()

from chris_martin_org.views import *

urlpatterns = patterns(
    '',
    url(r'^$', HomeView.as_view(), name='home'),
    url(r'^georgia-tech/?$', SchoolView.as_view(), name='school')
)
