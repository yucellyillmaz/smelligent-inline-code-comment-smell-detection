{\rtf1\ansi\ansicpg1254\cocoartf2822
\cocoatextscaling0\cocoaplatform0{\fonttbl\f0\fswiss\fcharset0 Helvetica;}
{\colortbl;\red255\green255\blue255;}
{\*\expandedcolortbl;;}
\paperw11900\paperh16840\margl1440\margr1440\vieww11520\viewh8400\viewkind0
\pard\tx720\tx1440\tx2160\tx2880\tx3600\tx4320\tx5040\tx5760\tx6480\tx7200\tx7920\tx8640\pardirnatural\partightenfactor0

\f0\fs24 \cf0 #!/usr/bin/env bash\
set -e\
\
mkdir -p inputs\
cd inputs\
\
git clone --depth 1 https://github.com/ankidroid/Anki-Android.git anki-android\
git clone --depth 1 https://github.com/jitsi/jitsi.git jitsi\
git clone --depth 1 https://github.com/square/moshi.git moshi\
git clone --depth 1 https://github.com/networknt/light-4j.git light-4j\
git clone --depth 1 https://github.com/psf/requests.git requests\
git clone --depth 1 https://github.com/scrapy/scrapy.git scrapy\
git clone --depth 1 https://github.com/kivy/kivy.git kivy\
git clone --depth 1 https://github.com/scikit-learn/scikit-learn.git scikit-learn}