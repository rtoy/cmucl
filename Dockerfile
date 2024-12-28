# Start from leverage tonistiigi/xx's Docker cross compilation helpers
FROM --platform=$BUILDPLATFORM tonistiigi/xx as xx

FROM --platform=$BUILDPLATFORM bash

RUN apk update && apk upgrade

ARG TARGETPLATFORM

WORKDIR /root

# add clang/lld for linux/amd64
RUN apk add clang lld

# add the cross-compilation support
COPY --from=xx / /

ENV TARGETPLATFORM linux/386

# add clang/lld for linux/386
RUN xx-apk add clang lld libgcc

# get the full glibc
RUN wget -q -O /i586-alpine-linux-musl/etc/apk/keys/sgerrand.rsa.pub https://alpine-pkgs.sgerrand.com/sgerrand.rsa.pub
RUN wget -q -O glibc-2.28-r0.apk https://github.com/sgerrand/alpine-pkg-glibc/releases/download/2.28-r0/glibc-2.28-r0.apk
RUN xx-apk add glibc-2.28-r0.apk

# add X support libraries
RUN xx-apk add motif-dev


ENV WORKDIR /usr/local/src/cmucl
ENV OLD_LISP_URL "https://common-lisp.net/project/cmucl/downloads/snapshots/2023/08"
ENV VERSION        "2023-08-x86"
ENV BOOTSTRP       "-B boot-2023-08"
ENV OLD_LISP_PATH "${WORKDIR}/snapshot"

# create the workdir
RUN mkdir -p "${WORKDIR}"

# create the place to install the working CMUCL
RUN mkdir "${OLD_LISP_PATH}"

WORKDIR "${OLD_LISP_PATH}"

# gather the working lisp
RUN wget -nv "${OLD_LISP_URL}/cmucl-${VERSION}-linux.tar.bz2"
RUN wget -nv "${OLD_LISP_URL}/cmucl-${VERSION}-linux.extra.tar.bz2"

# install the CMUCL image
RUN tar xjf "cmucl-${VERSION}-linux.tar.bz2"
RUN tar xjf "cmucl-${VERSION}-linux.extra.tar.bz2"


CMD ["bash"]