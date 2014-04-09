"""
Django settings for chris_martin_org project.
"""

import os
BASE_DIR = os.path.dirname(os.path.dirname(__file__))

PRODUCTION = bool(os.environ.get('PRODUCTION'))

DEBUG = not PRODUCTION

if PRODUCTION:
    SECRET_KEY = os.environ.get('DJANGO_SECRET_KEY') or ''
else:
    SECRET_KEY = 'gyyh@$kt*__ms4o8f9wnzebn!ou4p&^f0ubd^kc*fvs=-e=f*$'

TEMPLATE_DEBUG = True

TEMPLATE_DIRS = (
    os.path.join(BASE_DIR, 'chris_martin_org', 'templates'),
)

ALLOWED_HOSTS = [
    '.chris-martin.org',
    'chris-martin-org.herokuapp.com'
]

INSTALLED_APPS = (
    'django.contrib.contenttypes',
    'django.contrib.staticfiles',
    'chris_martin_org',
)

MIDDLEWARE_CLASSES = (
    'django.middleware.common.CommonMiddleware',
    'django.middleware.csrf.CsrfViewMiddleware',
    'django.middleware.clickjacking.XFrameOptionsMiddleware',
)

ROOT_URLCONF = 'chris_martin_org.urls'

WSGI_APPLICATION = 'chris_martin_org.wsgi.application'

import dj_database_url
DATABASES = {}
if PRODUCTION:
    DATABASES['default'] = dj_database_url.config()
else:
    DATABASES['default'] = {
        'ENGINE': 'django.db.backends.sqlite3',
        'NAME': os.path.join(BASE_DIR, 'db.sqlite3'),
    }

LANGUAGE_CODE = 'en-us'
TIME_ZONE = 'UTC'
USE_I18N = True
USE_L10N = True
USE_TZ = True

STATIC_URL = '/static/'
STATIC_ROOT = os.path.join(BASE_DIR, 'static')
STATICFILES_DIRS = (
    os.path.join(BASE_DIR, 'chris_martin_org', 'static'),
)

try:
    from .settings_local import *
except ImportError:
    pass
