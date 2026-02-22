(*:Various TList descendants, TList-compatible, and TList-similar classes.
   @author Primoz Gabrijelcic
   @desc <pre>

This software is distributed under the BSD license.

Copyright (c) 2024, Primoz Gabrijelcic
All rights reserved.

Redistribution and use in source and binary forms, with or without modification,
are permitted provided that the following conditions are met:
- Redistributions of source code must retain the above copyright notice, this
  list of conditions and the following disclaimer.
- Redistributions in binary form must reproduce the above copyright notice,
  this list of conditions and the following disclaimer in the documentation
  and/or other materials provided with the distribution.
- The name of the Primoz Gabrijelcic may not be used to endorse or promote
  products derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

   Author            : Primoz Gabrijelcic
   Creation date     : 2002-07-04
   Last modification : 2024-12-05
   Version           : 1.87a
</pre>*)(*
   History:
     1.87a: 2024-12-05
       - TGpIntegerObjectList.EnsureObject was broken since version 1.86.
     1.87: 2024-12-04
       - TGpFifoBuffer.Read can optionally return only whole source blocks.
     1.86a: 2024-11-28
       - Fixed: TGpFifoBuffer was not thread-safe.
     1.86: 2024-11-06
       - TGpIntegerObjectList and TGpInt64ObjectList override Ensure function
         to keep internal lists in sync.
     1.85a: 2023-11-28
       - Fixed potentially unitialized variable in TGpCache<K, V>.RemoveElement.
     1.85: 2023-08-21
       - Implemented TGpIntegerList.ToArray and TGpInt64List.ToArray.
     1.84: 2023-05-24
       - Added THeap<T>.
     1.83: 2023-04-05
       - Removed ARC support for Delphi 11+.
     1.82: 2023-02-14
       - Implemented TGpCache<K,V> enumerator.
     1.81: 2022-09-29
       - Added classes for calculating int64/uint64/extended moving averages.
     1.80a: 2020-10-06
       - Fixed incorrect casting in TGpObjectMap.
     1.80: 2020-03-25
       - Removed TGpStringListHelper.Sort as TStringList contains Sort in Delphi 10.3.
     1.79a: 2020-02-03
       - TGpInterfaceList<T>.Add did not set Result.
     1.79: 2019-03-21
       - Added default array property Values to TGpCache<K,V>.
       - Fixed TGpCache<K, V>.Update.
     1.78: 2019-03-20
       - Defined IGpCache<K,V>.
       - Added methods TGpCache<K,V>.Count, IsEmpty, PeekLRU, PeekMRU, RemoveLRU,
         and RemoveMRU.
     1.77: 2018-01-22
       - TGpFifoBuffer.Truncate was not taking active block into account.
     1.76: 2017-12-13
       - Successful TGpCache<K,V>.TryGetValue pushes retrieved key to the front of the
         MRU list.
     1.75: 2017-11-15
       - Key comparer can be set for TGpCache<K,V>.
     1.74: 2017-11-02
       - Implemented O(1) size-constrained cache TGpCache<K,V>.
     1.73: 2017-10-04
       - All lists that implemented Contains(element) got also Contains(element, var index).
         (Except the skip lists where elements are not indexed.)
     1.72c: 2017-09-05
       - TGpObjectMap's internal storage change from TGpIntegerObjectList to
         TGpInt64ObjectList so that it can correctly store pointers in 64-bit code.
     1.72b: 2017-04-06
       - Removed (useless, duplicate) GUID from IGpRingBuffer<T>.
     1.72a: 2016-08-30
       - TGpInt64ObjectList<T> methods had 'item' incorrectly defined as 'integer'
         instead of 'int64'.
     1.72: 2016-05-03
       - Implemented IGpInt64ObjectList<T> and TGpInt64ObjectList<T>.
     1.71: 2016-03-05
       - [bero] Added 'const' to various 'string' parameters.
       - [bero] Fixed invalid condition in GUIDCompare.
     1.70: 2016-03-04
       - TGpRingBuffer<T>.Dequeue: T returns Default(T) if queue is empty.
     1.69: 2016-02-24
       - Implemented TGpFifoBuffer.Clear.
     1.68: 2016-02-15
       - Implemented TGpObjectRingBuffer.BufferSizse.
     1.67: 2016-02-12
       - TGpFifoBuffer can maintain cached list of TFifoBlock objects.
     1.66: 2014-10-13
       - Implemented TGpDoublyLinkedList.EnumerateAs<T>.
     1.65: 2013-03-11
       - Implemented TGpSkipList<T>.Remove.
     1.64: 2012-12-07
       - TGpSkipList implements Head, Tail, Next and various overloads working with
         TGpSkipListEl<T>.
     1.63: 2012-12-05
       - Added missing TGpSkipList<T>.Locate.
       - Implemented TGpSkipObjectList<T>.
     1.62: 2012-11-21
       - Implemented TGpSkipList<T> and TGpSkipList<T,K>.
     1.61: 2012-07-04
       - Implemented IGpRingBuffer<T> and TGpRingBuffer<T>.
     1.60: 2012-05-07
       - Implemented TGpObjectRingBuffer.Remove.
     1.59: 2012-01-23
       - Added class TGpWideString.
     1.58: 2012-01-12
       - FifoBuffer supports external memory manager hooks (OnGetMem, OnFreeMem).
     1.57: 2012-01-11
       - (I|T)FifoBuffer renamed to (I|T)GpFifoBuffer.
       - Removed function CreateFifoBuffer, use TGpFifoBuffer.CreateInterface.
       - Speed optimizations in TGpDoublyLinkedList.
       - Inlining added to TGpFifoBuffer.
     1.56a: 2012-01-10
       - Fixed error in TGpFifoBuffer.Read (more data could be returned than requested).
       - Fixed error in TGpFifoBuffer.Write (DataSize/FifoPlace would return wrong value
         after failed write when the fifo was full).
     1.56: 2011-12-02
       - TGpIntegerObjectList<T> and TGpInterfaceList<T> are disabled in D2009
         due to the limitations of the compiler.
       - Fixed TGpIntegerObjectList<T>.Ensure(integer).
       - TGpInterfaceList<T> compiles with D2010.
     1.55: 2011-11-29
       - Each list implements class function CreateInterface which creates corresponding
         interface-implementing object.
     1.54: 2011-11-25
       - TGpIntegerObjectList implements IGpIntegerObjectList.
       - TGpIntegerObjectList<T> implements IGpIntegerObjectList<T>.
       - TGpCountedIntegerList implements IGpCountedIntegerList.
       - TGpInt64ObjectList implements IGpInt64ObjectList.
       - TGpCountedInt64List implements IGpCountedInt64List.
       - TGpGUIDList implements IGpGUIDList.
       - TGpTMethodList implements IGpTMethodList.
       - TGpClassList implements IGpClassList.
       - TGpObjectRingBuffer implements IGpObjectRingBuffer.
       - TGpObjectMap implements IGpObjectMap.
       - TGpObjectObjectMap implements IGpObjectObjectMap.
       - TGpDoublyLinkedList implements IGpDoublyLinkedList.
     1.53: 2011-11-24
       - TGpIntegerList implements IGpIntegerList.
       - TGpInt64List implements IGpInt64List.
     1.52: 2011-11-23
       - Added TGpIntegerObjectList<T>, a thin wrapper around TGpIntegerObjectList (D2009+).
       - Added TGpInterfaceList<T>, a thin wrapper aroung TList<T>, which implements
         IGpInterfaceList<T> (D2009+).
     1.51a: 2011-11-02
       - Find functions check (in Assert) if the list is sorted.
     1.51: 2011-06-21
       - Implemented limited size FIFO buffer.
     1.50: 2011-03-01
       - Added method FreeObjects to TGpIntegerObjectList and TGpInt64ObjectList.
     1.49: 2011-01-02
       - Implemented TGpGUIDList, a TGUID list.
     1.48b: 2011-01-01
       - [Erik Berry] TGpStringValue is only compiled if TStrings helper class is compiled.
     1.48a: 2010-10-21
       - Fixed TGpStringsHelper.WalkKV.
     1.48: 2010-10-19
       - Added method RemoveObject and enumerator WalkKV to the TStrings helper.
     1.47: 2010-10-13
       - Fixed TGp[Integer|Int64]List sorting (broken since 1.44 release).
     1.46a: 2010-07-28
       - [Jens] Capacity was not set to the ideal value in TGp[Integer|Int64]List.Append.
     1.46: 2010-07-13
       - [Istvan] Reintroduced Insert methods for Counted Integer and Int64 lists that
         accept a count parameter
     1.45: 2010-07-05
       - Added overloaded version of EnsureObject.
     1.44: 2010-05-13
       - TStringList helper split into TStrings and TStringList helpers.
     1.43: 2009-07-01
       - Added parameter 'step' to various Slice(), Walk() and WalkKV() enumerators.
     1.42: 2008-11-10
       - Added method FreeObjects to the TStringList helper.
     1.41: 2008-06-03
       - Unicode-ready (hope, hope).
     1.40: 2008-05-11
       - Delphi 7 compatibility restored.
       - Added helper method FetchObject to TGpIntegerObjectList,
         TGpInt64ObjectList, and TGpStringListHelper.
     1.39: 2008-04-22
       - Implemented TGpStringListHelper.Contains.
     1.38a: 2008-03-27
       - Fixed broken TGpInt64List.Move.
     1.38: 2008-03-25
       - Implemented method Sort in TGpIntegerList, TGpInt64List, and TGpStringListHelper.
     1.37: 2008-03-20
       - Added WalkKV enumerator to the TGpIntegerObjectList and TGpInt64ObjectList.
     1.36a: 2008-03-17
       - Use CUpperListBound instead of -1 as a Slice and Walk default value for the upper
         index.
     1.36: 2008-03-13
       - Added Walk enumerator to the TGpIntegerList and TGpInt64List. This enumerator
         allows list modifications (Add, Insert, Delete) from the enumeration consumer.
         IOW, you can do this:
         for idx in list.Walk do
           if SomeCondition(list[idx]) then
             list.Delete(idx);
     1.35: 2008-03-11
       - Modified TGpCountedInt64List to store int64 counters.
       - Added property ItemCounter[] to TGpCountedIntegerList and TGpCountedInt64List.
     1.34: 2008-03-03
       - Added Slice(from, to) enumerators to TGpIntegerList and TGpInt64List.
     1.33: 2007-11-27
       - Add TGpObjectRingBuffer and TGpDoublyLinkedList enumerators. Both lock access to
         the list during the enumeration process if multithreaded mode is enabled.
     1.32a: 2007-11-21
       - When TGpIntegerObjectList or TGpInt64ObjectList was Sorted and with Duplicates
         set to dupIgnore, calling AddObject with item that was already in the list caused
         internal exception. Test case:
           iol := TGpIntegerObjectList.Create(false);
           iol.Sorted := true;
           iol.Duplicates := dupIgnore;
           iol.AddObject(1, nil);
           iol.AddObject(1, nil);
     1.32: 2007-11-15
       - Added method Contains to TGpIntegerList, TGpInt64List, TGpCountedStringList,
         TGpTMethodList.
     1.31: 2007-10-26
       - Implemented TGpClassList, a TStringList-based list of classes. Class names must
         be unique. Useful when you need to implement a class registry to generate objects
         from class names.
       - Un-virtual-ized bunch of methods in TGpTMethodList so that they can be inlined.
       - Inlined some more getters and setters.
     1.30: 2007-10-18
       - Enumerators changed to records with GetCurrent inlined, as suggested in
         http://hallvards.blogspot.com/2007/10/more-fun-with-enumerators.html.
     1.29: 2007-10-03
       - Use spinlock for locking.
       - TGpObjectRingBuffer can put locks around all operations.
       - TGpObjectRingBuffer can trigger an event when buffer is fuller than the specified
         threshold and another event when buffer is emptier than the (different) threshold.
       - Added missing locks to TGpDoublyLinkedList in multithreaded mode.
     1.28b: 2007-09-13
       - Fixed Add operation on sorted lists (broken in 1.28a).
     1.28a: 2007-09-12
       - Disallow Move and Insert operations on sorted lists.
     1.28: 2007-07-25
       - Added Last method to the TStringList helper.
     1.27: 2007-06-28
       - Added bunch of 'inline' directives.
       - Added TStringList helper.
     1.26: 2007-04-17
       - Added TGpReal class.
     1.25: 2007-03-19
       - Added TGpCountedIntegerList class.
       - Added TGpCountedInt64List class.
       - Added CustomSort method to the TGpIntegerList and TGpInt64List classes.
     1.24: 2007-02-20
       - Added EqualTo method to the TGpIntegerList class.
     1.23a: 2007-01-19
       - Compiles with Delphi 6 again. Big thanks to Tao Lin and Erik Berry for reporting,
         diagnosing and fixing the problem.
     1.23: 2006-12-06
       - Added ValuesIdx to the TGpObjectMap class.
     1.22a: 2006-09-29
       - Fixed nasty bug in TGpIntegerObjectList.AddObject and
         TGpInt64ObjectList.AddObject.
       - Fixed range errors in TGpInt64[Object]List.
     1.22: 2006-09-20
       - Implemented TGpInt64List and TGpInt64ObjectList.
     1.21: 2006-05-15
       - Implemented list of TMethod records - TGpTMethodList.
     1.20: 2006-04-24
       - Added method TGpIntegerObjectList.ExtractObject.
     1.19: 2005-11-18
       - Added D2005-style TGpIntegerList enumerator.
     1.18: 2005-10-27
       - Added TGpString class.
     1.17: 2005-06-02
       - Added methods FreeAll and UnlinkAll to the TGpDoublyLinkedList class.
     1.16: 2004-11-22
       - Added Dump/Restore mechanism to the TGpInteger[Object]List classes.
     1.15: 2004-09-09
       - Added method Remove to the TGpIntegerList class.
     1.14: 2004-02-17
       - Added 'delimiter' parameter to the TGpIntegerList.AsHexText.
     1.13: 2004-02-12
       - Added iterator access (Count, Items) to the TGpObjectMap class.
     1.12: 2003-12-18
       - Published helper function IntegerCompare.
     1.11: 2003-11-05
       - TGpDoublyLinkedList got new constructor parameter - multithreaded. When
         set to True (default is False), all list-related operations are wrapped
         into a critical section.
     1.10: 2003-10-28
       - Added doubly-linked list class - TGpDoublyLinkedList.
     1.09a: 2003-10-16
       - TGpObjectRingBuffer.Head was off-by-one, causing all sorts of problems.
     1.09: 2003-09-27
       - Added function TGpIntegerList.AsDelimitedText.
     1.08: 2003-09-15
       - Added function TGpIntegerList.Ensure.
       - Added function TGpIntegerObjectList.EnsureObject.
       - Added function TGpCountedStringList.Ensure.
       - Added methods  TGpIntegerObjectList.LoadFromStream, .SaveToStream.
     1.07: 2003-08-02
       - Added class TGpObjectMap.
       - Added class TGpObjectObjectMap.
       - Added class TGpInt64.
     1.06: 2003-07-27
       - Prefixed all classes with 'Gp'.
       - Added class TGpObjectRingBuffer.
     1.05: 2003-07-15
       - Added overloaded constructor TIntegerList.Create(array of integer).
       - Added overloaded method Assign(array of integer).
     1.04: 2003-06-11
       - Added methods TIntegerList.SaveToStream and
         TIntegerList.LoadFromStream.
     1.03: 2003-06-09
       - Added TIntegerObjectList class.
     1.02a: 2003-03-21
       - Fixed TIntegerList.Find, which was completely broken.
     1.02: 2002-10-30
       - Added property TIntegerList.Text;
     1.01: 2002-09-23
       - Added method TIntegerList.IndexOf.
*)
unit GpIntegerLists;

{$mode objfpc}{$H+}

interface

uses
 Classes, Contnrs;

type
TGpIntegerList = class;
TGpIntegerListSortCompare = function(list: TGpIntegerList; Index1, Index2: integer): integer;

{ :List of integers.
  @since   2002-07-04.
}
TGpIntegerList = class
private
  ilDuplicates: TDuplicates;
  ilList: TList;
  ilSorted: boolean;
protected
  function GetAsDelimitedText(const delimiter: string; appendLastDelimiter: boolean): string;
  function GetCapacity: integer; virtual;
  function GetCount: integer; virtual;
  function GetItems(idx: integer): integer; virtual;
  function GetText: string; virtual;
  procedure InsertItem(idx, item: integer);
  function  GetDuplicates: TDuplicates; virtual;
  function  GetSorted: boolean; virtual;
  procedure QuickSort(L, R: integer; SCompare: TGpIntegerListSortCompare);
  procedure SetCapacity(const Value: integer); virtual;
  procedure SetCount(const Value: integer); virtual;
  procedure SetDuplicates(const value: TDuplicates); virtual;
  procedure SetItems(idx: integer; const Value: integer); virtual;
  procedure SetSorted(const Value: boolean); virtual;
  procedure SetText(const Value: string); virtual;
public
  constructor Create; overload;
  constructor Create(const elements: array of integer); overload;
  destructor Destroy; override;
  class function CreateFrom(list: TList): TGpIntegerList;
  function Add(item: integer): integer; virtual;
  procedure Append(const elements: array of integer); overload;
  procedure Append(list: TGpIntegerList); overload; virtual;
  function AsDelimitedText(const delimiter: string): string; {$IFDEF GpLists_Inline}inline; {$ENDIF}
  function AsHexText(const delimiter: string = ''): string;
  function AsString: string;
  procedure Assign(const elements: array of integer); overload;
  procedure Assign(list: TGpIntegerList); overload; virtual;
  procedure Clear; virtual;
  function  Contains(item: integer): boolean; overload;                   {$IFDEF GpLists_Inline}inline;{$ENDIF}
  function  Contains(item: integer; var idx: integer): boolean; overload; {$IFDEF GpLists_Inline}inline;{$ENDIF}
  function Lacks(item: integer): boolean; {$IFDEF GpLists_Inline}inline; {$ENDIF}
  function CountReducedBy(list: TGpIntegerList): integer;
  procedure CustomSort(sortMethod: TGpIntegerListSortCompare);
  procedure Delete(idx: integer); virtual;
  function Dump(baseAddr: pointer): pointer; virtual;
  function Ensure(item: integer): integer; virtual;
  function EqualTo(list: TGpIntegerList): boolean;
  procedure Exchange(idx1, idx2: integer); virtual;
  function Find(aValue: integer; var idx: integer): boolean; virtual;
  function First: integer; virtual;
  function IndexOf(item: integer): integer;
  procedure Insert(idx, item: integer); virtual;
  function Last: integer; virtual;
  function LoadFromStream(stream: TStream): boolean; virtual;
  procedure LoadFromFile(const FileName: string); virtual;
  procedure Move(curIdx, newIdx: integer); virtual;
  procedure Remove(item: integer); virtual;
  function RemoveDuplicates: integer;
  function Restore(baseAddr: pointer): pointer; virtual;
  procedure SaveToStream(stream: TStream); virtual;
  procedure SaveToFile(const FileName: string); virtual;
  procedure Sort; {$IFDEF GpLists_Inline}inline; {$ENDIF}
  property Capacity: integer read GetCapacity write SetCapacity;
  property Count: integer read GetCount write SetCount;
  property Duplicates: TDuplicates read ilDuplicates write ilDuplicates;
  property Items[idx: integer]: integer read GetItems write SetItems; default;
  property Sorted: boolean read ilSorted write SetSorted;
  property Text: string read GetText write SetText;
end; { TGpIntegerList }

{ :Integer list where each integer is accompanied with an object.
  @since   2003-06-09
}
TGpIntegerObjectList = class(TGpIntegerList)
private
  iolObjects: TObjectList;
protected
  function GetObject(idxObject: integer): TObject; virtual;
  procedure SetObject(idxObject: integer; const Value: TObject); virtual;
public
  constructor Create(ownsObjects: boolean = true);
  destructor Destroy; override;
  function Add(item: integer): integer; override;
  function AddObject(item: integer; obj: TObject): integer; virtual;
  procedure Clear; override;
  procedure Delete(idx: integer); override;
  function Dump(baseAddr: pointer): pointer; override;
  function  Ensure(item: integer): integer; override;
  function EnsureObject(item: integer; obj: TObject): integer; virtual;
  procedure Exchange(idx1, idx2: integer); override;
  function ExtractObject(idxObject: integer): TObject;
  function FetchObject(item: integer): TObject;
  procedure FreeObjects; virtual;
  procedure Insert(idx: integer; item: integer); override;
  procedure InsertObject(idx: integer; item: integer; obj: TObject); virtual;
  function LoadFromStream(stream: TStream): boolean; override;
  procedure Move(curIdx, newIdx: integer); override;
  function Restore(baseAddr: pointer): pointer; override;
  procedure SaveToStream(stream: TStream); override;
  property Objects[idxObject: integer]: TObject read GetObject write SetObject;
end; { TGpIntegerObjectList }


implementation

uses
  Math, SysUtils;

{:Useful helper function.
  @returns <0, 0, >0; depending on the result of the comparison
  @since   2003-12-18
}
function IntegerCompare(avalue1, avalue2: integer): integer;
begin
  if avalue1 < avalue2 then
    Result := -1
  else if avalue1 > avalue2 then
    Result := 1
  else
    Result := 0;
end; { IntegerCompare }

function IntegerListCompare(list: TGpIntegerList; idx1, idx2: integer): integer; {$IFDEF GpLists_Inline}inline; {$ENDIF}
begin
  Result := IntegerCompare(list[idx1], list[idx2]);
end; { IntegerListCompare }



{ TGpIntegerList }

constructor TGpIntegerList.Create;
begin
  inherited;
  ilList := TList.Create;
end; { TGpIntegerList.Create }

constructor TGpIntegerList.Create(const elements: array of integer);
begin
  Create;
  Assign(elements);
end; { TGpIntegerList.Create }

destructor TGpIntegerList.Destroy;
begin
  FreeAndNil(ilList);
  inherited;
end; { TGpIntegerList.Destroy }

function TGpIntegerList.Add(item: integer): integer;
begin
  if not Sorted then begin
    Result := ilList.Add(pointer(item));
  end
  else begin
    if Find(item, Result) then
      case Duplicates of
        dupIgnore: Exit;
        dupError : ilList.Error('List does not allow duplicates ($0%x)', item);
      end;
    InsertItem(Result, item);
  end;
end; { TGpIntegerList.Add }

procedure TGpIntegerList.Append(const elements: array of integer);
var
  iElement: integer;
begin
  SetCapacity(Count + Length(elements));
  for iElement := Low(elements) to High(elements) do
    Add(elements[iElement]);
end; { TGpIntegerList.Append }

procedure TGpIntegerList.Append(list: TGpIntegerList);
var
  iItem: integer;
begin
  SetCapacity(Count + list.Count);
  for iItem := 0 to list.Count-1 do
    Add(list[iItem]);
end; { TGpIntegerList.Append }

function TGpIntegerList.AsDelimitedText(const delimiter: string): string;
begin
  Result := GetAsDelimitedText(delimiter, false);
end; { TGpIntegerList.AsDelimitedText }

function TGpIntegerList.AsHexText(const delimiter: string): string;
var
  hsl  : TStringList;
  iItem: integer;
begin
  hsl := TStringList.Create;
  try
    for iItem := 0 to Count-2 do
      hsl.Add(Format('%x', [Items[iItem]]));
    Result := hsl.Text;
    if delimiter <> '' then
      Result := StringReplace(Result, #13#10, delimiter, [rfReplaceAll]);
    if Count > 0 then
      Result := Result + Format('%x', [Items[Count-1]]);
  finally FreeAndNil(hsl); end;
end; { TGpIntegerList.AsHexText }

procedure TGpIntegerList.Assign(const elements: array of integer);
begin
  Clear;
  Append(elements);
end; { TGpIntegerList.Assign }

procedure TGpIntegerList.Assign(list: TGpIntegerList);
begin
  Clear;
  Append(list);
end; { TGpIntegerList.Assign }

procedure TGpIntegerList.Clear;
begin
  ilList.Clear;
end; { TGpIntegerList.Clear }

function TGpIntegerList.Contains(item: integer; var idx: integer): boolean;
begin
  idx := IndexOf(item);
  Result := (idx >= 0);
end; { TGpIntegerList.Contains }

function TGpIntegerList.Contains(item: integer): boolean;
var
  idx: integer;
begin
  Result := Contains(item, idx);
end; { TGpIntegerList.Contains }

procedure TGpIntegerList.CustomSort(sortMethod: TGpIntegerListSortCompare);
begin
  if not Sorted and (Count > 1) then
    QuickSort(0, Count - 1, sortMethod);
end; { TGpIntegerList.CustomSort }

procedure TGpIntegerList.Delete(idx: integer);
begin
  ilList.Delete(idx);
end; { TGpIntegerList.Delete }

{:Dumps the list into memory block starting at baseAddr.
  @returns Pointer to the byte immediately after the end of dumped data.
  @since   2004-11-22
}
function TGpIntegerList.Dump(baseAddr: pointer): pointer;
var
  iList: integer;
  pList: PDWORD;
begin
  pList := baseAddr;
  pList^ := Count;
  Inc(pList);
  for iList := 0 to Count-1 do begin
    pList^ := DWORD(Items[iList]);
    Inc(pList);
  end;
  Result := pList;
end; { TGpIntegerList.Dump }

function TGpIntegerList.Ensure(item: integer): integer;
begin
  Result := IndexOf(item);
  if Result < 0 then
    Result := Add(item);
end; { TGpIntegerList.Ensure }

///<summary>Checks whether two lists contain equal elements.</summary>
///<returns>True if elements in all positions do match.</returns>
///<since>2007-02-18</since>
function TGpIntegerList.EqualTo(list: TGpIntegerList): boolean;
var
  iList: integer;
begin
  Result := Count = list.Count;
  if Result then begin
    for iList := 0 to Count - 1 do
      if Items[iList] <> list.GetItems(iList) then begin
        Result := false;
        break; //for iList
      end;
  end;
end; { TGpIntegerList.EqualTo }

procedure TGpIntegerList.Exchange(idx1, idx2: integer);
begin
  ilList.Exchange(idx1, idx2);
end; { TGpIntegerList.Exchange }

function TGpIntegerList.Find(avalue: integer; var idx: integer): boolean;
var
  L, H, I, C: integer;
begin
  Assert(ilSorted, 'Find only works on sorted lists!');
  Result := false;
  L := 0;
  H := Count - 1;
  while L <= H do begin
    I := (L + H) shr 1;
    C := IntegerCompare(Items[I], avalue);
    if C < 0 then
      L := I + 1
    else begin
      H := I - 1;
      if C = 0 then begin
        Result := true;
        if Duplicates <> dupAccept then
          L := I;
      end;
    end;
  end;
  idx := L;
end; { TGpIntegerList.Find }

function TGpIntegerList.First: integer;
begin
  Result := Items[0];
end; { TGpIntegerList.First }

function TGpIntegerList.GetAsDelimitedText(const delimiter: string;
  appendLastDelimiter: boolean): string;
var
  iItem   : integer;
  item    : integer;
  lenDelim: integer;
  lenItem : integer;
  p       : PChar;
  q       : PChar;
  sItem   : string;
  size    : integer;
begin
  size := 0;
  lenDelim := Length(delimiter);
  for iItem := 0 to Count-1 do begin
    item := GetItems(iItem);
    if item = 0 then
      lenItem := 1
    else if item < 0 then
      lenItem := Trunc(Log10(-item))+2
    else
      lenItem := Trunc(Log10(item))+1;
    Inc(size, lenItem);
    Inc(size, lenDelim);
  end;
  if not appendLastDelimiter then
    Dec(size, lenDelim);
  SetString(Result, nil, size);
  p := Pointer(Result);
  for iItem := 0 to Count-1 do begin
    sItem := IntToStr(GetItems(iItem));
    lenItem := Length(sItem);
    if lenItem <> 0 then begin
      System.Move(pointer(sItem)^, p^, lenItem*SizeOf(char));
      Inc(p, lenItem);
    end;
    if appendLastDelimiter or (iItem < (Count-1)) then begin
      q := Pointer(delimiter);
      while q^ <> #0 do begin
        p^ := q^;
        Inc(p);
        Inc(q);
      end; //while
    end;
  end;
end; { TGpIntegerList.GetAsDelimitedText }

function TGpIntegerList.GetCapacity: integer;
begin
  Result := ilList.Capacity;
end; { TGpIntegerList.GetCapacity }

function TGpIntegerList.GetCount: integer;
begin
  Result := ilList.Count;
end; { TGpIntegerList.GetCount }

function TGpIntegerList.GetDuplicates: TDuplicates;
begin
  Result := ilDuplicates;
end; { TGpIntegerList.GetDuplicates }

{$IFDEF GpLists_Enumerators}
function TGpIntegerList.GetEnumerator: TGpIntegerListEnumerator;
begin
  Result := TGpIntegerListEnumerator.Create(Self, 0, Count - 1, 1);
end; { TGpIntegerList.GetEnumerator }
{$ENDIF GpLists_Enumerators}

function TGpIntegerList.GetItems(idx: integer): integer;
begin
  Result := integer(ilList.Items[idx]);
end; { TGpIntegerList.GetItems }

function TGpIntegerList.GetSorted: boolean;
begin
  Result := ilSorted;
end; { TGpIntegerList.GetSorted }

function TGpIntegerList.GetText: string;
begin
  Result := GetAsDelimitedText(#13#10, true);
end; { TGpIntegerList.GetText }

function TGpIntegerList.IndexOf(item: integer): integer;
begin
  if Sorted then begin
    if not Find(item, Result) then
      Result := -1
  end
  else
    Result := ilList.IndexOf(pointer(item));
end; { TGpIntegerList.IndexOf }

procedure TGpIntegerList.Insert(idx, item: integer);
begin
  if Sorted then
    raise Exception.Create('Cannot insert element in sorted list.');
  InsertItem(idx, item);
end; { TGpIntegerList.Insert }

procedure TGpIntegerList.InsertItem(idx, item: integer);
begin
  ilList.Insert(idx, pointer(item));
end; { TGpIntegerList.InsertItem }

function TGpIntegerList.Last: integer;
begin
  Result := Items[Count-1];
end; { TGpIntegerList.Last }

function TGpIntegerList.LoadFromStream(stream: TStream): boolean;
var
  item: integer;
  read: integer;
begin
  Result := false;
  Clear;
  repeat
    read := stream.Read(item, 4);
    if read = 4 then
      Add(item)
    else if read > 0 then
      Exit;
  until read = 0;
  Result := true;
end; { TGpIntegerList.LoadFromStream }

procedure TGpIntegerList.Move(curIdx, newIdx: integer);
begin
  if Sorted then
    raise Exception.Create('Cannot move elements in sorted list.');
  ilList.Move(curIdx, newIdx);
end; { TGpIntegerList.Move }

procedure TGpIntegerList.QuickSort(L, R: integer; SCompare: TGpIntegerListSortCompare);
var
  I, J, P: integer;
begin
  repeat
    I := L;
    J := R;
    P := (L + R) shr 1;
    repeat
      while (SCompare(Self, I, P) < 0) do
        Inc(I);
      while (SCompare(Self, J, P) > 0) do
        Dec(J);
      if (I <= J) then begin
        Exchange(I, J);
        if (P = I) then
          P := J
        else if (P = J) then
          P := I;
        Inc(I);
        Dec(J);
      end;
    until (I > J);
    if (L < J) then
      QuickSort(L, J, SCompare);
    L := I;
  until (I >= R);
end; { TGpIntegerList.QuickSort }

procedure TGpIntegerList.Remove(item: integer);
var
  idxItem: integer;
begin
  idxItem := IndexOf(item);
  if idxItem >= 0 then
    Delete(idxItem);
end; { TGpIntegerList.Remove }

{:Restores the list dumped by the Dump method.
  @returns Pointer to the byte immediately after the end of dumped data.
  @since   2004-11-22
}
function TGpIntegerList.Restore(baseAddr: pointer): pointer;
var
  iList   : integer;
  numItems: integer;
  pList   : PDWORD;
begin
  pList := baseAddr;
  numItems := integer(pList^);
  Inc(pList);
  ilList.Capacity := numItems;
  for iList := 0 to numItems-1 do begin
    Add(integer(pList^));
    Inc(pList);
  end;
  Result := pList;
end; { TGpIntegerList.Restore }

procedure TGpIntegerList.SaveToStream(stream: TStream);
var
  iItem: integer;
  item : integer;
begin
  for iItem := 0 to Count-1 do begin
    item := Items[iItem];
    stream.WriteBuffer(item, 4);
  end;
end; { TGpIntegerList.SaveToStream }

procedure TGpIntegerList.SetCapacity(const value: integer);
begin
  ilList.Capacity := value;
end; { TGpIntegerList.SetCapacity }

procedure TGpIntegerList.SetCount(const value: integer);
begin
  ilList.Count := value;
end; { TGpIntegerList.SetCount }

procedure TGpIntegerList.SetDuplicates(const value: TDuplicates);
begin
  ilDuplicates := value;
end; { TGpIntegerList.SetDuplicates }

procedure TGpIntegerList.SetItems(idx: integer; const value: integer);
begin
  ilList.Items[idx] := pointer(value);
end; { TGpIntegerList.SetItems }

procedure TGpIntegerList.SetSorted(const value: boolean);
begin
  if ilSorted <> value then begin
    if value then
      CustomSort(@IntegerListCompare);
    ilSorted := value;
  end;
end; { TGpIntegerList.SetSorted }


procedure TGpIntegerList.SetText(const value: string);
var
  p    : PChar;
  s    : string;
  start: PChar;
begin
  Clear;
  p := pointer(value);
  if P <> nil then
    while p^ <> #0 do begin
      start := p;
      while (p^ <> #0) and (p^ <> #10) and (p^ <> #13) do
        Inc(p);
      SetString(s, start, p - start);
      Add(StrToInt(s));
      if p^ = #13 then Inc(p);
      if p^ = #10 then Inc(p);
    end;
end; { TGpIntegerList.SetText }

{$IFDEF GpLists_Enumerators}
function TGpIntegerList.Slice(idxFrom, idxTo, step: integer): TGpIntegerListSliceEnumeratorFactory;
begin
  if idxTo = CUpperListBound then
    idxTo := Count - 1;
  Result := TGpIntegerListSliceEnumeratorFactory.Create(Self, idxFrom, idxTo, step);
end; { TGpIntegerList.Slice }
{$ENDIF GpLists_Enumerators}

procedure TGpIntegerList.Sort;
begin
  Sorted := false;
  Sorted := true;
end; { TGpIntegerList.Sort }

function TGpIntegerList.AsString: string;
var
  n: integer;
begin
  if (Count = 0) then
  begin
    Result := 'NULL';
  end
  else
  begin
    Result := '';
    for n := 0 to pred(Count) do
    begin
      if (n > 0) then
        Result := Result + ',';
      Result := Result + IntToStr(Items[n]);
    end;
    Result := '(' + Result + ')';
  end;
end;

class function TGpIntegerList.CreateFrom(list: TList): TGpIntegerList;
var
  n: integer;
begin
  Result := TGpIntegerList.Create;
  for n := 0 to pred(list.Count) do
    Result.Add(integer(list[n]));
end;

function TGpIntegerList.Lacks(item: integer): boolean;
begin
  Result := (IndexOf(item) = -1);
end; { TGpIntegerList.Contains }

function TGpIntegerList.CountReducedBy(list: TGpIntegerList): integer;
var
  n: integer;
begin
  Result := Count;
  for n := 0 to pred(list.Count) do
    if (IndexOf(list[n]) <> -1) then
      Dec(Result);
end; { TGpIntegerList.CountReducedBy }

procedure TGpIntegerList.LoadFromFile(const FileName: string);
var
  f: TFileStream;
begin
  f := TFileStream.Create(FileName, fmOpenRead);
  LoadFromStream(f);
  f.free;
end;

function TGpIntegerList.RemoveDuplicates: integer;
var
  n: integer;
begin
  Result := 0;
  Sort;
  for n := pred(Count) downto 1 do
    if Items[n] = Items[pred(n)] then
    begin
      Delete(n);
      Inc(Result);
    end;
end;

procedure TGpIntegerList.SaveToFile(const FileName: string);
var
  f: TFileStream;
begin
  f := TFileStream.Create(FileName, fmCreate);
  SaveToStream(f);
  f.free;
end;

{ TGpIntegerObjectList }

constructor TGpIntegerObjectList.Create(ownsObjects: boolean);
begin
  inherited Create;
  iolObjects := TObjectList.Create(ownsObjects);
end; { TGpIntegerObjectList.Create }

destructor TGpIntegerObjectList.Destroy;
begin
  FreeAndNil(iolObjects);
  inherited;
end; { TGpIntegerObjectList.Destroy }

function TGpIntegerObjectList.Add(item: integer): integer;
begin
  Result := AddObject(item, nil);
end; { TGpIntegerObjectList.Add }

function TGpIntegerObjectList.AddObject(item: integer; obj: TObject): integer;
begin
  {$IFDEF GpLists_Sorting}
  if Sorted and (Duplicates = dupIgnore) then begin
    Result := IndexOf(item);
    if Result >= 0 then begin
      Objects[Result] := obj;
      Exit;
    end;
  end;
  {$ENDIF}
  Result := inherited Add(item);
  iolObjects.Insert(Result, obj);
  Assert(Count = iolObjects.Count,
    'TGpIntegerObjectList.AddObject: Number of items and objects differ');
end; { TGpIntegerObjectList.AddObject }

procedure TGpIntegerObjectList.Clear;
begin
  inherited;
  iolObjects.Clear;
end; { TGpIntegerObjectList.Clear }


procedure TGpIntegerObjectList.Delete(idx: integer);
begin
  inherited;
  iolObjects.Delete(idx);
  Assert(Count = iolObjects.Count,
    'TGpIntegerObjectList.Delete: Number of items and objects differ');
end; { TGpIntegerObjectList.Delete }

{:Dumps the list into memory block starting at baseAddr.
  @returns Pointer to the byte immediately after the end of dumped data.
  @since   2004-11-22
}
function TGpIntegerObjectList.Dump(baseAddr: pointer): pointer;
var
  iList: integer;
  pList: PDWORD;
begin
  pList := baseAddr;
  pList^ := Count;
  Inc(pList);
  for iList := 0 to Count-1 do begin
    pList^ := DWORD(Items[iList]);
    Inc(pList);
    pList^ := DWORD(PtrUInt(Objects[iList]));
    Inc(pList);
  end;
  Result := pList;
end; { TGpIntegerObjectList.Dump }

function TGpIntegerObjectList.Ensure(item: integer): integer;
begin
  Result := EnsureObject(item, nil);
end; { TGpIntegerObjectList }

function TGpIntegerObjectList.EnsureObject(item: integer; obj: TObject): integer;
begin
  Result := IndexOf(item);
  if Result < 0 then
    Result := AddObject(item, obj)
  else if not assigned(Objects[Result]) then
    Objects[Result] := obj;
end; { TGpIntegerObjectList.EnsureObject }

{$IFDEF UNICODE}
function TGpIntegerObjectList.EnsureObject(item: integer; objClass: TClass): integer;
begin
  Result := IndexOf(item);
  if Result < 0 then
    Result := AddObject(item, objClass.Create())
  else if not assigned(Objects[Result]) then
    Objects[Result] := objClass.Create();
end; { TGpIntegerObjectList.EnsureObject }
{$ENDIF UNICODE}

procedure TGpIntegerObjectList.Exchange(idx1, idx2: integer);
begin
  inherited;
  iolObjects.Exchange(idx1, idx2);
end; { TGpIntegerObjectList.Exchange }

function TGpIntegerObjectList.ExtractObject(idxObject: integer): TObject;
begin
  Result := Objects[idxObject];
  iolObjects.Extract(iolObjects[idxObject]);
  inherited Delete(idxObject);
end; { TGpIntegerObjectList.ExtractObject }

function TGpIntegerObjectList.FetchObject(item: integer): TObject;
var
  idxItem: integer;
begin
  idxItem := IndexOf(item);
  if idxItem >= 0 then
    Result := Objects[idxItem]
  else
    Result := nil;
end; { TGpIntegerObjectList.FetchObject }

procedure TGpIntegerObjectList.FreeObjects;
var
  iObject: integer;
begin
  for iObject := 0 to Count - 1 do begin
    Objects[iObject].Free;
    Objects[iObject] := nil;
  end;
end; { TGpIntegerObjectList.FreeObjects }

function TGpIntegerObjectList.GetObject(idxObject: integer): TObject;
begin
  Result := iolObjects[idxObject];
end; { TGpIntegerObjectList.GetObject }

procedure TGpIntegerObjectList.Insert(idx, item: integer);
begin
  InsertObject(idx, item, nil);
end; { TGpIntegerObjectList.Insert }

procedure TGpIntegerObjectList.InsertObject(idx, item: integer; obj: TObject);
begin
  inherited Insert(idx, item);
  iolObjects.Insert(idx, obj);
  Assert(Count = iolObjects.Count,
    'TGpIntegerObjectList.InsertObject: Number of items and objects differ');
end; { TGpIntegerObjectList.InsertObject }

function TGpIntegerObjectList.LoadFromStream(stream: TStream): boolean;
var
  item: integer;
  obj : TObject;
  read: integer;
begin
  Result := false;
  Clear;
  repeat
    read := stream.Read(item, 4);
    if read = 0 then
      break; //repeat
    if read <> 4 then
      Exit;
    read := stream.Read(obj, 4);
    if read <> 4 then
      Exit;
    AddObject(item, obj)
  until read = 0;
  Result := true;
end; { TGpIntegerObjectList.LoadFromStream }

procedure TGpIntegerObjectList.Move(curIdx, newIdx: integer);
begin
  inherited;
  iolObjects.Move(curIdx, newIdx);
end; { TGpIntegerObjectList.Move }

{:Restores the list dumped by the Dump method.
  @returns Pointer to the byte immediately after the end of dumped data.
  @since   2004-11-22
}
function TGpIntegerObjectList.Restore(baseAddr: pointer): pointer;
var
  iList   : integer;
  item    : integer;
  numItems: integer;
  pList   : PDWORD;
begin
  pList := baseAddr;
  numItems := integer(pList^);
  Inc(pList);
  ilList.Capacity := numItems;
  iolObjects.Capacity := numItems;
  for iList := 0 to numItems-1 do begin
    item := integer(pList^);
    Inc(pList);
    AddObject(item, TObject(PtrUInt(pList^)));
    Inc(pList);
  end;
  Result := pList;
end; { TGpIntegerObjectList.Restore }

procedure TGpIntegerObjectList.SaveToStream(stream: TStream);
var
  iItem: integer;
  item : integer;
  obj  : TObject;
begin
  for iItem := 0 to Count-1 do begin
    item := Items[iItem];
    stream.WriteBuffer(item, 4);
    obj := Objects[iItem];
    stream.WriteBuffer(obj, 4);
  end;
end; { TGpIntegerObjectList.SaveToStream }

procedure TGpIntegerObjectList.SetObject(idxObject: integer; const value: TObject);
begin
  iolObjects[idxObject] := value;
end; { TGpIntegerObjectList.SetObject }


end.
