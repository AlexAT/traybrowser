(*
Licensed under BSD 3-clause license
https://opensource.org/license/bsd-3-clause

Copyright 2025-2026 Alex/AT (alex@alex-at.net)

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:
1. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
2. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
3. Neither the name of the copyright holder nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.
THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS “AS IS” AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*)

{$MODE OBJFPC}{$MODESWITCH ADVANCEDRECORDS}{$H+}{$CODEPAGE UTF8}
{$MACRO ON}

// {$DEFINE TB_DEBUG_ZMQ} - enables ZMQ-based protocol debugging
// {$DEFINE TB_DEBUG_JS_CALL} - enables JS call API debugging
// {$DEFINE TB_DEBUG} - enables general debugging
// {$DEFINE TB_INIT_DEBUG} - enables initialization/termination debugging that cannot use CEF debug.log to be written to local tbdebug.log file
// {$DEFINE TB_TRACE_FEE} - enables detailed routine entry/exit tracing for major routines
// {$DEFINE TB_TRACE_FEE_ZMQMSG} - enables entry/exit tracing for frequently called ZMQ messaging routines
// {$DEFINE TB_TRACE_FEE_INSANE} - enables insane level of routine entry/exit tracing by including all minor routines that are called every now and then

